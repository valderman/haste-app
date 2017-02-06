{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
-- | Base type for Haste.App native servers.
module Haste.App.Server.Type where
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Proxy
import Data.Typeable
import Haste.Concurrent
import Haste.App.Protocol.Types

-- | A server type, providing the base for more advanced, custom servers.
--   In order to make a simple single-server application, creating an
--   appropriate instance of 'Node' for 'Server' is all that's needed.
type Server = EnvServer ()

-- | Invoke a standard server. This is the 'invoke' method when
--   creating 'Node' instances for @EnvServer@.
invokeServer :: e -> EnvServer e a -> CIO a
invokeServer env = flip runReaderT env . runEnvS

-- | A local endpoint using its type fingerprint as its identifier.
localEndpoint :: Typeable m => Proxy m -> Endpoint
localEndpoint = LocalNode . show . typeRepFingerprint . typeRep

-- | A server type with an environment.
newtype EnvServer e a = EnvS {runEnvS :: ReaderT e CIO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadConc, MonadReader e)

instance MonadConc (ReaderT e CIO) where
  liftCIO = lift . liftCIO
  fork m = lift . fork . runReaderT m =<< ask
