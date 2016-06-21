{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
module App
  ( Endpoint (..) , Node (..)
  , ServerException (..)
  , Client, Server, MonadBlob (..), MonadIO (..)
  , remote, import_, runApp
  ) where
import Remote
import Client
import Server
import Protocol
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class
import Haste.Binary
import Haste.Concurrent (concurrent, fork)

-- | Run a Haste.App application. On the client side, a thread is forked off
--   to run the client part in isolation.
--   On the server side, one connection handler thread is forked off for each
--   endpoint. To conserve system resources, it is recommended to build one
--   server-side binary for each intended endpoint, with a single endpoint
--   handler for each. However, it is perfectly possible to build a single
--   server-side binary to handle *all* endpoints, and run that binary on
--   multiple machines.
runApp :: [Endpoint] -> Client () -> IO ()
#ifdef __HASTE__
runApp _ = concurrent . fork . runClient
#endif
-- Another #if instead of #else, as haskell-mode breaks horribly on #else.
#ifndef __HASTE__
runApp eps' _ = mapM_ (forkIO . serverLoop) eps >> eternalSlumber
  where
    eternalSlumber = threadDelay (30*60*1000000) >> eternalSlumber
    eps' = snub [port | Endpoint _ port <- eps]
    snub = map head . group . sort
#endif

-- | A server type, providing the base for more advanced, custom servers.
--   In order to make a simple single-server application, creating an
--   appropriate instance of 'Node' for 'Server' is all that's needed.
newtype Server a = Server (IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadBlob Server where
  -- Server-side, Blob and BlobData are just different newtypes around the same
  -- ByteString.
#ifndef __HASTE__
  getBlobData = pure . unsafeCoerce
  getBlobText' = pure . toJSStr . toString . unsafeFromBlob
#else
  getBlobData = pure . const undefined
  getBlobText' = pure . const undefined
#endif
