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
import Haste.Concurrent

import Data.Typeable
import GHC.StaticPtr
import Haste.App.Client
import Haste.App.Protocol
import Haste.App.Routing as Routing
import Haste.App.Sandbox

newtype Import (m :: * -> *) dom = Import (Routing.Env m -> [JSON] -> CIO JSON)

type family Result a where
  Result (a -> b) = Result b
  Result (m a)    = m

-- | Server-side type of a 'Remotable' function.
type family Remote m a where
  Remote m (a -> b)   = (a -> Remote m b)
  Remote m (Client a) = m a

-- | A client-side frontend for a 'Dispatch' function.
class Node m => Remotable m a where
  -- | Plumbing for turning a 'StaticKey' into a remote function, callable on
  --   the client.
  dispatch' :: Proxy m -> StaticKey -> [JSON] -> a

instance (Serialize a, Remotable m b) => Remotable m (a -> b) where
  dispatch' pm k xs x = dispatch' pm k (toJSON x : xs)

instance forall m a. (Tunnel Client (ClientOf m), Node m, Serialize a)
         => Remotable m (Client a) where
  dispatch' pm k xs = do
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

-- | Invoke a remote function: send the RPC call over the network and wait for
--   the response to get back.
{-# WARNING call "TODO: share code between sandbox/websocket endpoints" #-}
call :: Tunnel Client server
     => Proxy (server :: * -> *) -> StaticKey -> [JSON] -> Client JSON
call pm k xs = do
    let (ep, _) = mkPacket 0
    case ep of
      LocalNode _ -> do
        n <- getNonce
        liftCIO $ uncurry (callSandbox n) $ mkPacket n
      WebSocket{} -> do
        (n, v) <- newResult
        uncurry sendOverWS $ mkPacket n
        liftCIO $ takeMVar v
  where
    mkPacket n =
      tunnel (Proxy :: Proxy Client) pm $ ServerCall
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
dispatch :: forall m dom. (Remotable m (H dom), Mapping m (Res dom))
         => StaticPtr (Import m dom)
         -> H dom
dispatch f = dispatch' (Proxy :: Proxy m) (staticKey f) []

type family H a where
  H (a -> b) = (a -> H b)
  H (m a)    = Client (Hask m a)

-- | The result type of a monadic function.
type family Res a where
  Res (a -> b) = Res b
  Res (m a)    = a
