{-# LANGUAGE TypeFamilies,
             ScopedTypeVariables,
             FlexibleInstances,
             MultiParamTypeClasses,
             FlexibleContexts,
             DefaultSignatures,
             TypeOperators,
             ConstraintKinds #-}
module Haste.App.Remote where
import Haste.Serialize
import Haste.JSON
import qualified Haste.JSString as S
import Haste.Concurrent
import Data.Proxy

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
  ( Node (Affinity dom)
  , Allowed (Affinity dom) (Affinity cli)
  , Remotable cli
  , HaskF (Affinity cli) dom ~ cli
  )

-- | Any function type @dom@ which can be exported from a node to another.
type Export dom =
  ( Remote (Affinity dom) dom
  , Serialize (Hask (Affinity dom) (Res dom))
  )

-- | A client-side frontend for a 'Remote' function.
class Remotable a where
  -- | Plumbing for turning a 'StaticKey' into a remote function, callable on
  --   the client.
  dispatch' :: Endpoint -> StaticKey -> [JSON] -> a

instance {-# OVERLAPPING #-} (Serialize a, Remotable b) => Remotable (a -> b) where
  dispatch' ep k xs x = dispatch' ep k (toJSON x : xs)

instance (MonadClient cli, Serialize a) => Remotable (cli a) where
  dispatch' ep k xs = do
    Right x <- fromJSON <$> call ep k (reverse xs)
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
      _        -> error "impossible: JSON conversion failed"
  blob _ _ _ = error "impossible: too few arguments"

instance (Affinity (m a) ~ m, Mapping m a, Res (m a) ~ a) => Remote m (m a) where
  blob m _ env = invoke env m

call :: MonadClient from => Endpoint -> StaticKey -> [JSON] -> from JSON
call ep k xs = do
  n <- getNonce
  remoteCall ep (encodeJSON $ toJSON $ (ServerCall n k xs)) n

-- | Serializify any function of type @a -> ... -> b@ into a corresponding
--   function of type @[JSON] -> Server JSON@, with the same semantics.
--   This allows the function to be called remotely via a static pointer.
remote :: forall dom. Export dom => dom -> Import dom
remote f = Import $ \env xs ->
  toJSON <$> (blob f xs env :: CIO (Hask (Affinity dom) (Res dom)))

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
dispatch :: forall cli dom. Dispatch dom cli => StaticPtr (Import dom) -> cli
dispatch f = dispatch' ep (staticKey f) []
  where ep = endpoint (Proxy :: Proxy (Affinity dom))

-- | Like 'dispatch', but makes a direct call to the server, and overrides
--   the its default endpoint. The server node must be directly attached to
--   the client making the call.
dispatchTo :: Dispatch dom cli => Endpoint -> StaticPtr (Import dom) -> cli
dispatchTo ep f = dispatch' ep (staticKey f) []
