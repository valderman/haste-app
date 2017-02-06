{-# LANGUAGE TypeFamilies,
             ScopedTypeVariables,
             FlexibleInstances,
             MultiParamTypeClasses,
             FlexibleContexts,
             DefaultSignatures,
             FunctionalDependencies,
             UndecidableInstances #-}
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

type family Result a where
  Result (a -> b) = Result b
  Result (m a)    = m

-- | A client-side frontend for a 'Dispatch' function.
class Tunnel cli m => Remotable (cli :: * -> *) (m :: * -> *) a where
  -- | Plumbing for turning a 'StaticKey' into a remote function, callable on
  --   the client.
  dispatch' :: Proxy cli -> Proxy m -> StaticKey -> [JSON] -> a

instance (Serialize a, Remotable cli m b) => Remotable cli m (a -> b) where
  dispatch' pc pm k xs x = dispatch' pc pm k (toJSON x : xs)

instance forall cli m a.
            (MonadClient cli, Tunnel cli (ClientOf m), Node m, Serialize a)
         => Remotable cli m (cli a) where
  dispatch' _ pm k xs = do
    Right x <- fromJSON <$> call pm k (reverse xs)
    return x

-- | A function that may act as a server-side callback. That is, one where all
--   arguments and return values are serializable.
class (Result dom ~ m) => Callback m dom where
  -- | Serializify a function so it may be called remotely.
  blob :: dom -> [JSON] -> Routing.Env m -> CIO (Hask m (Res dom))

instance (Serialize a, Callback m b, Mapping m (Res b)) => Callback m (a -> b) where
  blob f (x:xs) env =
    case fromJSON x of
      Right x' -> blob (f x') xs env
  blob _ _ _ = error "too few arguments to remote function"

instance (Result (m a) ~ m, Mapping m a, Res (m a) ~ a) => Callback m (m a) where
  blob m _ env = invoke env m

call :: forall server m. (Tunnel m server, MonadClient m)
     => Proxy server -> StaticKey -> [JSON] -> m JSON
call pm k xs = do
    n <- getNonce
    let (ep, pkt) = mkPacket n
    remoteCall ep pkt n
  where
    mkPacket n =
      tunnel (Proxy :: Proxy m) pm $ ServerCall
        { scNonce  = n
        , scMethod = k
        , scArgs   = xs
        }

-- | Serializify any function of type @a -> ... -> b@ into a corresponding
--   function of type @[JSON] -> Server JSON@, with the same semantics.
--   This allows the function to be called remotely via a static pointer.
remote :: forall m dom.
          (Node m, Callback m dom, Mapping m (Res dom), Serialize (Hask m (Res dom)))
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
dispatch :: forall m cli dom. (Remotable (Result cli) m cli, Mapping m (Res dom), H (Result cli) dom ~ cli)
         => StaticPtr (Import m dom)
         -> cli
dispatch f = dispatch' (Proxy :: Proxy (Result cli)) (Proxy :: Proxy m) (staticKey f) []

type family H c a where
  H c (a -> b) = (a -> H c b)
  H c (m a)    = c a

-- | The result type of a monadic function.
type family Res a where
  Res (a -> b) = Res b
  Res (m a)    = a
