{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, FlexibleContexts, ScopedTypeVariables #-}
module Haste.App
  ( Endpoint (..), Node (..)
  , MonadConc (..), MonadIO (..)
  , Callback, Remotable, Remote, RunsOn, remote, dispatch, annotate
  , Client, Server, ServerException (..), Proxy (..), NodeConfig
  , runApp, start, invokeServer, reconnect, onDisconnect, onReconnect
  , using
  ) where
import Control.Monad.IO.Class
import Data.Proxy
import Haste.Serialize
import Haste.App.Remote
import Haste.App.Client
import Haste.App.Protocol
import Haste.App.Routing
import Haste.Concurrent (MonadConc, CIO, concurrent, fork)

#ifndef __HASTE__
import Haste.App.Server
import Control.Concurrent (forkIO, threadDelay)
import Data.List
import Unsafe.Coerce
import Haste.Prim
import Data.ByteString.Lazy.UTF8
import System.IO
#endif

import GHC.StaticPtr

-- | Start a server of the given node when this server binary starts.
start :: forall m. Node m => Proxy m -> NodeConfig
#ifdef __HASTE__
start _ = pure ()
#else
start p = do
  case endpoint p of
    Endpoint _ port -> do
      env <- Haste.App.Routing.init p
      -- TODO: adapt this part so it works for client-side nodes as well
      liftIO $ serverLoop (NodeEnv env :: NodeEnv m) port
    _ -> do
      return ()
#endif

-- | Run a Haste.App application. On the client side, a thread is forked off
--   to run the client part in isolation.
--   On the server side, one connection handler thread is forked off for each
--   endpoint. To conserve system resources, it is recommended to build one
--   server-side binary for each intended endpoint, with a single endpoint
--   handler for each. However, it is perfectly possible to build a single
--   server-side binary to handle *all* endpoints, and run that binary on
--   multiple machines.
runApp :: [NodeConfig] -> Client () -> IO ()
#ifdef __HASTE__
runApp _ = concurrent . fork . runClient
#else
runApp eps _ = mapM_ (forkIO . concurrent) eps >> zzz
  where zzz = threadDelay (30*60*1000000) >> zzz
#endif

-- | A server type, providing the base for more advanced, custom servers.
--   In order to make a simple single-server application, creating an
--   appropriate instance of 'Node' for 'Server' is all that's needed.
newtype Server a = Server {invokeServer :: CIO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadConc)

-- | Force the type of a monadic computation. Used to annotate inline remote
--   imports.
type RunsOn m = m ()

-- | Annotate a monadic computation with the node it's intended to run on.
--   This is often necessary when doing inline imports:
--
--       reverse_ :: String -> Client String
--       reverse_ = dispatch $ static (remote $ \x -> do
--           annotate :: RunsOn Server
--           return (reverse x)
--         )
--
--   This is essentially a more readable way to say @return () :: Server ()@.
annotate :: Monad m => RunsOn m
annotate = return ()

-- | Convenience function to use inline remote blocks.
--   Since remote functions rely on static pointers, inline blocks must not have
--   any free variables. Instead, this function explicitly captures any free
--   variables and constructs an explicit closure.
--
--   An example of usage:
-- > example :: Client ()
-- > example = do
-- >   name <- prompt "What's your name?"
-- >   age <- prompt "What's your age?"
-- >   using (name, age) $ static (remote $ \(name, age) -> do
-- >       annotate :: RunsOn Server
-- >       JSString.putStrLn (JSString.concat ["name is ", age, " years old"])
-- >     )
using :: forall m a b. (Serialize a, Remotable m b)
        => a -> (StaticPtr (Import m (a -> Remote m b))) -> b
using fv f = dispatch f fv
