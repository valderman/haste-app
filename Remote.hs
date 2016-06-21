{-# LANGUAGE StaticPointers, TypeFamilies, ScopedTypeVariables, GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, CPP #-}
module Remote where
import Haste.Binary
import Haste.Concurrent

import Control.Monad.IO.Class
import Data.Typeable
import GHC.StaticPtr
import Client
import Protocol (Endpoint (..))

#ifndef __HASTE__
import Unsafe.Coerce
import Data.ByteString.Lazy.UTF8 (toString)
import Server (unsafeFromBlob)
import Haste.Prim (toJSStr)
#endif

newtype Import m a = Import ([Blob] -> m Blob)

type family Remote m a where
  Remote m (a -> b) = (a -> Remote m b)
  Remote m (m a)    = Client a

type family ServerMonad m where
  ServerMonad (a -> b) = ServerMonad b
  ServerMonad (m a)    = m

-- | A node which a client may ask to perform some computation.
class MonadBlob m => Node m where
  endpoint :: m a -> Endpoint

-- | Any function which can be called remotely from the client.
class (ServerMonad a ~ m, Node m) => Remotable m a where
  -- | Plumbing for turning a 'StaticKey' into a remote function, callable on
  --   the client.
  remote' :: a -> StaticKey -> [Blob] -> Remote m a

  -- | Serializify a function so it may be called remotely.
  blob :: a -> [Blob] -> m Blob

instance (Binary a, Remotable m b) => Remotable m (a -> b) where
  remote' _ k xs x = remote' (undefined :: b) k (encode x : xs)
  blob f (x:xs) = do
    Right x' <- decodeBlob x
    blob (f x') xs

instance ( m ~ ServerMonad (m a)
         , Remote m (m a) ~ Client a
         , Node m
         , Binary a
         ) => Remotable m (m a) where
  remote' m k xs = do
    Right x <- decodeBlob =<< invoke (endpoint m) k (reverse xs)
    return x
  blob m _ = fmap encode m

-- | Serializify any function of type @a -> ... -> b@ into a corresponding
--   function of type @[Blob] -> Server Blob@, with the same semantics.
--   This allows the function to be called remotely via a static pointer.
import_ :: Remotable m a => a -> Import m a
import_ = Import . blob

-- | Turn a static pointer to a serializified function into a client-side
--   function which, when fully applied, is executed on the server.
--
--   The full, somewhat unwieldy, incantation to import a server-side
--   function @f@ to the client reads:
--
--       remote $ static (import_ f)
remote :: forall a m. Remotable m a => StaticPtr (Import m a) -> Remote m a
remote f = remote' (undefined :: a) (staticKey f) []
