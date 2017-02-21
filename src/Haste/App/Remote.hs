{-# LANGUAGE TypeFamilies,
             ScopedTypeVariables,
             FlexibleInstances,
             MultiParamTypeClasses,
             FlexibleContexts,
             DefaultSignatures,
             ConstraintKinds #-}
module Haste.App.Remote where
import Haste.Serialize
import Haste.JSON
import qualified Haste.JSString as S
import Haste.Concurrent

import Data.Typeable
import GHC.StaticPtr
import Haste.App.Client
import Haste.App.Protocol
import Haste.App.Routing as Routing
import Haste.App.Sandbox
import Haste.App.Transport

newtype Import dom = Import (Routing.Env (Affinity dom) -> [JSON] -> CIO JSON)

-- | The affinity, i.e. node type, of a function.
type family Affinity a where
  Affinity (a -> b) = Affinity b
  Affinity (m a)    = m

-- | The result type of a function on some node.
type family Res a where
  Res (a -> b) = Res b
  Res (m a)    = a

-- | The Haskell equivalent type of a domain-specific function.
type family HaskF c a where
  HaskF c (a -> b) = (a -> HaskF c b)
  HaskF c (m a)    = c (Hask m a)

-- | Any types @m@, @cli@ and @dom@ such that @cli@ is a function in the
--   client monad, @m@ is the type of the server node, and @dom@ is a server
--   computation that is type-compatible with @cli@.
type Dispatch dom cli =
  ( Remotable (Affinity cli) (Affinity dom) cli
  , Mapping (Affinity dom) (Res dom)
  , HaskF (Affinity cli) dom ~ cli
  )

-- | Any function type @dom@ which can be exported from a node to another.
type Export dom =
  ( Node (Affinity dom)
  , Remote (Affinity dom) dom
  , Mapping (Affinity dom) (Res dom)
  , Serialize (Hask (Affinity dom) (Res dom))
  )

-- | Does the function @f@ execute on an immediate child of @n@?
type ChildOf c p = Affinity p ~ Parent (Affinity c)

-- | A client-side frontend for a 'Remote' function.
class Tunnel cli m => Remotable (cli :: * -> *) (m :: * -> *) a where
  -- | Plumbing for turning a 'StaticKey' into a remote function, callable on
  --   the client.
  dispatch' :: Maybe Endpoint -> Proxy cli -> Proxy m -> StaticKey -> [JSON] -> a

instance (Serialize a, Remotable cli m b) => Remotable cli m (a -> b) where
  dispatch' me pc pm k xs x = dispatch' me pc pm k (toJSON x : xs)

instance forall cli m a.
            (Call cli m, Tunnel cli (Parent m), Node m, Serialize a)
         => Remotable cli m (cli a) where
  dispatch' me _ pm k xs = do
    Right x <- fromJSON <$> call me pm k (reverse xs)
    return x

-- | A function that may act as a server-side callback. That is, one where all
--   arguments and return values are serializable.
class (Affinity dom ~ m) => Remote m dom where
  -- | Serializify a function so it may be called remotely.
  blob :: dom -> [JSON] -> Routing.Env m -> CIO (Hask m (Res dom))

instance (Serialize a, Remote m b) => Remote m (a -> b) where
  blob f (x:xs) env =
    case fromJSON x of
      Right x' -> blob (f x') xs env
  blob _ _ _ = error "too few arguments to remote function"

instance (Affinity (m a) ~ m, Mapping m a, Res (m a) ~ a) => Remote m (m a) where
  blob m _ env = invoke env m

class MonadClient from => Call from to where
  call :: (Tunnel from to, MonadClient from)
       => Maybe Endpoint -> Proxy to -> StaticKey -> [JSON] -> from JSON

instance (Node n, MonadClient n) => Call n n where
  call _ _ k xs = do
    env <- getEnv
    Just p <- liftIO $ unsafeLookupStaticPtr k
    liftCIO $ deRefStaticPtr p env xs

instance {-# OVERLAPPABLE #-} MonadClient from => Call from to where
  call me pm k xs = do
      n <- getNonce
      case me of
        Just ep -> remoteCall ep (encodeJSON $ toJSON $ mkCall n) n
        _       -> let (ep, pkt) = mkPacket n in remoteCall ep pkt n
    where
      mkCall n = ServerCall
        { scNonce  = n
        , scMethod = k
        , scArgs   = xs
        }
      mkPacket = tunnel (Proxy :: Proxy from) pm . mkCall

-- | Serializify any function of type @a -> ... -> b@ into a corresponding
--   function of type @[JSON] -> Server JSON@, with the same semantics.
--   This allows the function to be called remotely via a static pointer.
remote :: forall dom. Export dom
       => dom
       -> Import dom
remote f = Import $ \env xs -> toJSON <$> (blob f xs env :: CIO (Hask (Affinity dom) (Res dom)))

-- | Turn a static pointer to a serializified function into a client-side
--   function which, when fully applied, is executed on the server.
--
--   The full, somewhat unwieldy, incantation to import a server-side
--   function @f@ to the client reads:
--
-- > dispatch $ static (remote f)
--
--   Of course, the dispatch may be used as part of the invocation, rather than
--   definition, of the import instead:
--
-- > f' = static (remote f)
-- > main = runApp $ dispatch f' x0 x1 ...
dispatch :: forall cli dom. Dispatch dom cli
         => StaticPtr (Import dom)
         -> cli
dispatch f = dispatch' Nothing (Proxy :: Proxy (Affinity cli)) (Proxy :: Proxy (Affinity dom)) (staticKey f) []

-- | Like 'dispatch', but makes a direct call to the server, and overrides
--   the its default endpoint. The server node must be directly attached to
--   the client making the call.
dispatchTo :: forall cli dom. (Dispatch dom cli, ChildOf dom cli)
           => Endpoint
           -> StaticPtr (Import dom)
           -> cli
dispatchTo e f = dispatch' (Just e) (Proxy :: Proxy (Affinity cli)) (Proxy :: Proxy (Affinity dom)) (staticKey f) []
