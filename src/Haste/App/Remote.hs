{-# LANGUAGE TypeFamilies,
             ScopedTypeVariables,
             FlexibleInstances,
             MultiParamTypeClasses,
             FlexibleContexts,
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

newtype Import (m :: * -> *) a = Import (Routing.Env m -> [JSON] -> CIO JSON)

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
class (Result a ~ m) => Callback m a where
  -- | Serializify a function so it may be called remotely.
  blob :: a -> [JSON] -> m JSON

instance (Serialize a, Callback m b) => Callback m (a -> b) where
  blob f (x:xs) =
    case fromJSON x of
      Right x' -> blob (f x') xs
  blob _ _ = error "too few arguments to remote function"

instance (Result (m a) ~ m, Functor m, Serialize a) => Callback m (m a) where
  blob m _ = fmap toJSON m

-- | Invoke a remote function: send the RPC call over the network and wait for
--   the response to get back.
call :: Tunnel Client server
     => Proxy (server :: * -> *) -> StaticKey -> [JSON] -> Client JSON
call pm k xs = do
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
remote :: (Node m, Callback m a) => a -> Import m a
remote f = Import $ \env xs -> invoke env (blob f xs)

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
dispatch :: forall a m. Remotable m a => StaticPtr (Import m (Remote m a)) -> a
dispatch f = dispatch' (Proxy :: Proxy m) (staticKey f) []
