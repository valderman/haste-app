{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
module Haste.App.V2
  ( EndpointConfig (..), Endpoint (..), Node (..)
  , MonadConc (..), MonadIO (..)
  , Callback, Remotable, Remote, RunsOn, remote, import_, annotate
  , Client, Server, ServerException (..), Proxy (..)
  , runApp, invokeServer, reconnect, onDisconnect, onReconnect
  ) where
import Control.Monad.IO.Class
import Data.Proxy
import Haste.Binary
import Haste.App.V2.Remote
import Haste.App.V2.Client
import Haste.App.V2.Config
import Haste.App.V2.Protocol
import Haste.App.V2.Routing
import Haste.Concurrent (MonadConc, CIO, concurrent, fork)

#ifndef __HASTE__
import Haste.App.V2.Server
import Control.Concurrent (forkIO, threadDelay)
import Data.List
import Unsafe.Coerce
import Haste.Prim
import Data.ByteString.Lazy.UTF8
#endif

-- | Run a Haste.App application. On the client side, a thread is forked off
--   to run the client part in isolation.
--   On the server side, one connection handler thread is forked off for each
--   endpoint. To conserve system resources, it is recommended to build one
--   server-side binary for each intended endpoint, with a single endpoint
--   handler for each. However, it is perfectly possible to build a single
--   server-side binary to handle *all* endpoints, and run that binary on
--   multiple machines.
runApp :: [EndpointConfig] -> Client () -> IO ()
#ifdef __HASTE__
runApp _ = concurrent . fork . runClient
#else
runApp eps _ = mapM_ (forkIO . uncurry serverLoop) ports >> eternalSlumber
  where
    eternalSlumber = threadDelay (30*60*1000000) >> eternalSlumber
    ports = snub [(port, tls) | Endpoint _ port tls <- map resolveEndpoint eps]
    snub = map head . group . sort
#endif

-- | A server type, providing the base for more advanced, custom servers.
--   In order to make a simple single-server application, creating an
--   appropriate instance of 'Node' for 'Server' is all that's needed.
newtype Server a = Server {invokeServer :: CIO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadConc, MonadBlob)

-- | Force the type of a monadic computation. Used to annotate inline remote
--   imports.
type RunsOn m = m ()

-- | Annotate a monadic computation with the node it's intended to run on.
--   This is often necessary when doing inline imports:
--
--       reverse_ :: String -> Client String
--       reverse_ = remote $ static (import_ $ \x -> do
--           annotate :: RunsOn Server
--           return (reverse x)
--         )
--
--   This is essentially a more readable way to say @return () :: Server ()@.
annotate :: Monad m => RunsOn m
annotate = return ()