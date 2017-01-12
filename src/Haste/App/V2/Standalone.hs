{-# LANGUAGE OverloadedStrings, CPP #-}
-- | Create standalone Haste.App applications, which don't require a web server
--   or static Haste.App server/port configuration.
module Haste.App.V2.Standalone
  ( module Haste.App.V2
  , EndpointConfig (..)
  , runStandaloneApp
  ) where
import Haste.App.V2
#ifdef __HASTE__
import Control.Monad (void)
import Haste
#else
import Haste.App.V2.Standalone.Server
#endif

-- | Run a Haste.App application in standalone mode: host/port configuration
--   is read from the command line, and a local web server is started to serve
--   the client program JS and any associated static files.
runStandaloneApp :: [EndpointConfig] -> Client () -> IO ()
#ifdef __HASTE__
runStandaloneApp eps app = void $ setTimer (Once 0) $ runApp eps app
#else
runStandaloneApp eps _ = runStandaloneServer eps
#endif
