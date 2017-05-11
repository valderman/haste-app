{-# LANGUAGE ScopedTypeVariables #-}
-- | Data transport handling for a nodes capable of acting as clients.
module Haste.App.Transport
  ( MonadClient (..)
  , getNonce
  ) where
import Control.Monad.IO.Class
import Data.IORef
import Data.Proxy
import Data.Typeable
import Haste.JSON (JSON)
import Haste.Concurrent (MonadConc)
import qualified Haste.JSString as S
import System.IO.Unsafe
import Haste.App.Protocol

{-# NOINLINE nonceRef #-}
nonceRef :: IORef Nonce
nonceRef = unsafePerformIO $ newIORef 0

-- | Get a nonce that's guaranteed to be unique per physical machine, modulo
--   overflow. By extension, this means that the nonce is guaranteed to be
--   unique per node as well.
getNonce :: MonadIO m => m Nonce
getNonce = liftIO $ atomicModifyIORef' nonceRef $ \n -> (n+1, n)

class (Typeable m, MonadConc m) => MonadClient m where
  -- | Invoke a remote function: send the RPC call over the network and wait for
  --   the response to get back.
  --   The message received from the server will be a 'ServerReply'. Instances
  --   of this class must return the JSON within that reply.
  remoteCall :: Endpoint -> S.JSString -> Nonce -> m JSON
