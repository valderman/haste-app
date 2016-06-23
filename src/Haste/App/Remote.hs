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

newtype Import a = Import ([Blob] -> IO Blob)

type family Remote m a where
  Remote m (a -> b) = (a -> Remote m b)
  Remote m (m a)    = Client a

type family ServerMonad m where
  ServerMonad (a -> b) = ServerMonad b
  ServerMonad (m a)    = m

-- | Constraint signifying that a computation of type @f@ can be executed on
--   node @m@, which is connected to the client along path @path@.
type ConnectedNode m path f =
  ( m ~ ServerMonad (m f)
  , Remote m (m f) ~ Client f
  , Tunnel (Path Client m)
  , Path Client m ~ (m ': path)
  , Node m
  , MonadBlob m
  )

-- | Any function which can be called remotely from the client.
class (ServerMonad a ~ m, MonadBlob m, Node m) => Remotable m a where
  -- | Plumbing for turning a 'StaticKey' into a remote function, callable on
  --   the client.
  remote' :: Proxy a -> StaticKey -> [Blob] -> Remote m a

  -- | Serializify a function so it may be called remotely.
  blob :: a -> [Blob] -> m Blob

instance (Binary a, Remotable m b) => Remotable m (a -> b) where
  remote' _ k xs x = remote' (Proxy :: Proxy b) k (encode x : xs)
  blob f (x:xs) = do
    Right x' <- decodeBlob x
    blob (f x') xs

instance forall m path a. (ConnectedNode m path a, Binary a)
         => Remotable m (m a) where
  remote' m k xs = do
    Right x <- decodeBlob =<< call (Proxy :: Proxy m) k (reverse xs)
    return x
  blob m _ = fmap encode m

-- | Invoke a remote function: send the RPC call over the network and wait for
--   the response to get back.
call :: forall (server :: * -> *) path.
          ( (server ': path) ~ Path Client server
          , Tunnel (Path Client server)
          )
         =>
           Proxy (server :: * -> *) -> StaticKey -> [Blob] -> Client Blob
call pm k xs = do
    (n, v) <- newResult
    uncurry sendOverWS $ mkPacket n
    liftCIO $ takeMVar v
  where
    mkPacket n =
      fmap encode $ tunnel (Proxy :: Proxy (Path Client server)) $ ServerCall
        { scNonce  = n
        , scMethod = k
        , scArgs   = xs
        }

-- | Serializify any function of type @a -> ... -> b@ into a corresponding
--   function of type @[Blob] -> Server Blob@, with the same semantics.
--   This allows the function to be called remotely via a static pointer.
import_ :: Remotable m a => a -> Import a
import_ f = Import $ \x -> invoke (blob f x)

-- | Turn a static pointer to a serializified function into a client-side
--   function which, when fully applied, is executed on the server.
--
--   The full, somewhat unwieldy, incantation to import a server-side
--   function @f@ to the client reads:
--
--       remote $ static (import_ f)
remote :: forall a m. Remotable m a => StaticPtr (Import a) -> Remote m a
remote f = remote' (Proxy :: Proxy a) (staticKey f) []
