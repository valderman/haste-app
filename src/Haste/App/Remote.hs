{-# LANGUAGE StaticPointers,
             TypeFamilies,
             ScopedTypeVariables,
             FlexibleInstances,
             MultiParamTypeClasses,
             CPP,
             TypeOperators,
             DataKinds,
             FlexibleContexts,
             UndecidableInstances,
             ConstraintKinds #-}
module Haste.App.Remote where
import Haste.Binary
import Haste.Concurrent

import Control.Monad.IO.Class
import Data.Typeable
import GHC.StaticPtr
import Haste.App.Client
import Haste.App.Protocol
import Haste.App.Routing

#ifndef __HASTE__
import Unsafe.Coerce
import Data.ByteString.Lazy.UTF8 (toString)
import Haste.App.Server (unsafeFromBlob)
import Haste.Prim (toJSStr)
#endif

newtype Import (m :: * -> *) a = Import ([Blob] -> IO Blob)

type family Result a where
  Result (a -> b) = Result b
  Result (m a)    = m

type family Remote m a where
  Remote m (a -> b)   = (a -> Remote m b)
  Remote m (Client a) = m a

-- | Any function which can be called remotely from the client.
class Node m => Remotable m a where
  -- | Plumbing for turning a 'StaticKey' into a remote function, callable on
  --   the client.
  remote' :: Proxy m -> Proxy a -> StaticKey -> [Blob] -> a

class (Result a ~ m, MonadBlob m) => Blobby m a where
  -- | Serializify a function so it may be called remotely.
  blob :: a -> [Blob] -> m Blob

instance (Binary a, Remotable m b) => Remotable m (a -> b) where
  remote' pm _ k xs x = remote' pm (Proxy :: Proxy b) k (encode x : xs)

instance (Binary a, Blobby m b) => Blobby m (a -> b) where
  blob f (x:xs) = do
    Right x' <- decodeBlob x
    blob (f x') xs

instance forall m a. (Tunnel Client (ClientOf m), Node m, Binary a)
         => Remotable m (Client a) where
  remote' pm _ k xs = do
    Right x <- decodeBlob =<< call pm k (reverse xs)
    return x

instance (Result (m a) ~ m, MonadBlob m, Binary a) => Blobby m (m a) where
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
      fmap encode $ tunnel (Proxy :: Proxy Client) pm $ ServerCall
        { scNonce  = n
        , scMethod = k
        , scArgs   = xs
        }

-- | Serializify any function of type @a -> ... -> b@ into a corresponding
--   function of type @[Blob] -> Server Blob@, with the same semantics.
--   This allows the function to be called remotely via a static pointer.
import_ :: (Node m, Blobby m a) => a -> Import m a
import_ f = Import $ \x -> invoke (blob f x)

-- | Turn a static pointer to a serializified function into a client-side
--   function which, when fully applied, is executed on the server.
--
--   The full, somewhat unwieldy, incantation to import a server-side
--   function @f@ to the client reads:
--
--       remote $ static (import_ f)
remote :: forall a m. Remotable m a => StaticPtr (Import m (Remote m a)) -> a
remote f = remote' (Proxy :: Proxy m) (Proxy :: Proxy a) (staticKey f) []