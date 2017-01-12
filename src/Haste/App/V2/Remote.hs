{-# LANGUAGE TypeFamilies,
             ScopedTypeVariables,
             FlexibleInstances,
             MultiParamTypeClasses,
             FlexibleContexts,
             UndecidableInstances #-}
module Haste.App.V2.Remote where
import Haste.Binary
import Haste.Concurrent

import Data.Typeable
import GHC.StaticPtr
import Haste.App.V2.Client
import Haste.App.V2.Protocol
import Haste.App.V2.Routing

newtype Import (m :: * -> *) a = Import ([Blob] -> CIO Blob)

type family Result a where
  Result (a -> b) = Result b
  Result (m a)    = m

-- | Server-side type of a 'Remotable' function.
type family Remote m a where
  Remote m (a -> b)   = (a -> Remote m b)
  Remote m (Client a) = m a

-- | A client-side frontend for a 'Remote' function.
class Node m => Remotable m a where
  -- | Plumbing for turning a 'StaticKey' into a remote function, callable on
  --   the client.
  remote' :: Proxy m -> StaticKey -> [Blob] -> a

instance (Binary a, Remotable m b) => Remotable m (a -> b) where
  remote' pm k xs x = remote' pm k (encode x : xs)

instance forall m a. (Tunnel Client (ClientOf m), Node m, Binary a)
         => Remotable m (Client a) where
  remote' pm k xs = do
    Right x <- decodeBlob =<< call pm k (reverse xs)
    return x

-- | A function that may act as a server-side callback. That is, one where all
--   arguments and return values are serializable.
class (Result a ~ m, MonadConc m, MonadBlob m) => Callback m a where
  -- | Serializify a function so it may be called remotely.
  blob :: a -> [Blob] -> m Blob

instance (Binary a, Callback m b) => Callback m (a -> b) where
  blob f (x:xs) = do
    Right x' <- decodeBlob x
    blob (f x') xs
  blob _ _ = error "too few arguments to remote function"

instance (Result (m a) ~ m, MonadConc m, MonadBlob m, Binary a) => Callback m (m a) where
  blob m _ = fmap encode m

-- | Invoke a remote function: send the RPC call over the network and wait for
--   the response to get back.
call :: Tunnel Client server
     => Proxy (server :: * -> *) -> StaticKey -> [Blob] -> Client Blob
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
--   function of type @[Blob] -> Server Blob@, with the same semantics.
--   This allows the function to be called remotely via a static pointer.
import_ :: (Node m, Callback m a) => a -> Import m a
import_ f = Import $ \x -> invoke (blob f x)

-- | Turn a static pointer to a serializified function into a client-side
--   function which, when fully applied, is executed on the server.
--
--   The full, somewhat unwieldy, incantation to import a server-side
--   function @f@ to the client reads:
--
--       remote $ static (import_ f)
remote :: forall a m. Remotable m a => StaticPtr (Import m (Remote m a)) -> a
remote f = remote' (Proxy :: Proxy m) (staticKey f) []
