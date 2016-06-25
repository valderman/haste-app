{-# LANGUAGE OverloadedStrings, CPP #-}
-- | Create standalone Haste.App applications, which don't require a web server
--   or static Haste.App server/port configuration.
module Haste.App.Standalone
  ( module Haste.App
  , EndpointConfig (..)
  , runStandaloneApp
  ) where
import Haste.App
#ifdef __HASTE__
import Control.Monad (void)
import Haste
import Haste.Foreign
#else
import Haste.App.Standalone.Server
#endif
import System.IO.Unsafe

-- | Run a Haste.App application in standalone mode: host/port configuration
--   is read from the command line, and a local web server is started to serve
--   the client program JS and any associated static files.
runStandaloneApp :: [EndpointConfig] -> Client () -> IO ()
#ifdef __HASTE__
runStandaloneApp eps app = void $ setTimer (Once 0) $ runApp eps app
#else
runStandaloneApp eps _ = runStandaloneServer eps
#endif
