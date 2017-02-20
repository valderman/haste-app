{-# LANGUAGE TypeFamilies,
             ScopedTypeVariables,
             FlexibleInstances,
             MultiParamTypeClasses,
             FlexibleContexts,
             DefaultSignatures,
             FunctionalDependencies,
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

newtype Import (m :: * -> *) dom = Import (Routing.Env m -> [JSON] -> CIO JSON)

-- | The identity, i.e. node type, of a function.
type family Ident a where
  Ident (a -> b) = Ident b
  Ident (m a)    = m

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
type Dispatch m cli dom =
  ( Remotable (Ident cli) m cli
  , Mapping m (Res dom)
  , HaskF (Ident cli) dom ~ cli
  )

-- | Does the function @f@ execute on an immediate child of @n@?
type ChildOf f n = Ident f ~ Parent n

-- | A client-side frontend for a 'Dispatch' function.
class Tunnel cli m => Remotable (cli :: * -> *) (m :: * -> *) a where
  -- | Plumbing for turning a 'StaticKey' into a remote function, callable on
  --   the client.
  dispatch' :: Maybe Endpoint -> Proxy cli -> Proxy m -> StaticKey -> [JSON] -> a

instance (Serialize a, Remotable cli m b) => Remotable cli m (a -> b) where
  dispatch' me pc pm k xs x = dispatch' me pc pm k (toJSON x : xs)

instance forall cli m a.
            (MonadClient cli, Tunnel cli (Parent m), Node m, Serialize a)
         => Remotable cli m (cli a) where
  dispatch' me _ pm k xs = do
    Right x <- fromJSON <$> call me pm k (reverse xs)
    return x

-- | A function that may act as a server-side callback. That is, one where all
--   arguments and return values are serializable.
class (Ident dom ~ m) => Remote m dom where
  -- | Serializify a function so it may be called remotely.
  blob :: dom -> [JSON] -> Routing.Env m -> CIO (Hask m (Res dom))

instance (Serialize a, Remote m b, Mapping m (Res b)) => Remote m (a -> b) where
  blob f (x:xs) env =
    case fromJSON x of
      Right x' -> blob (f x') xs env
  blob _ _ _ = error "too few arguments to remote function"

instance (Ident (m a) ~ m, Mapping m a, Res (m a) ~ a) => Remote m (m a) where
  blob m _ env = invoke env m

call :: forall server m. (Tunnel m server, MonadClient m)
     => Maybe Endpoint -> Proxy server -> StaticKey -> [JSON] -> m JSON
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
    mkPacket = tunnel (Proxy :: Proxy m) pm . mkCall

-- | Serializify any function of type @a -> ... -> b@ into a corresponding
--   function of type @[JSON] -> Server JSON@, with the same semantics.
--   This allows the function to be called remotely via a static pointer.
remote :: forall m dom.
          (Node m, Remote m dom, Mapping m (Res dom), Serialize (Hask m (Res dom)))
       => dom
       -> Import m dom
remote f = Import $ \env xs -> toJSON <$> (blob f xs env :: CIO (Hask m (Res dom)))

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
dispatch :: forall m cli dom. Dispatch m cli dom
         => StaticPtr (Import m dom)
         -> cli
dispatch f = dispatch' Nothing (Proxy :: Proxy (Ident cli)) (Proxy :: Proxy m) (staticKey f) []

-- | Like 'dispatch', but makes a direct call to the server, and overrides
--   the its default endpoint. The server node must be directly attached to
--   the client making the call.
dispatchTo :: forall m cli dom. (Dispatch m cli dom, ChildOf cli m)
           => Endpoint
           -> StaticPtr (Import m dom)
           -> cli
dispatchTo e f = dispatch' (Just e) (Proxy :: Proxy (Ident cli)) (Proxy :: Proxy m) (staticKey f) []
