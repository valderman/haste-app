{-# LANGUAGE TypeFamilies #-}
-- | Boilerplate for writing simple applications that only need a single
--   server component, as with the original Haste.App described in.
--   <http://haste-lang.org/pubs/haskell14.pdf>.
--   Such applications may still use any number
--   of external endpoints -- REST APIs, for instance -- however.
--
--   See <https://github.com/valderman/haste-app/blob/master/README.md> for
--   a short tutorial on getting started with Haste.App using this module.
module Haste.App.Simple
  ( module Haste
  , module Haste.App.Standalone
  , module Haste.Events
  , module Haste.DOM.JSString
  , runSimpleApp, simpleEndpoint
  ) where
import Data.Proxy
import Haste
import Haste.App.Config (resolveEndpoint)
import Haste.App.Standalone
import Haste.DOM.JSString
import Haste.Events

instance Node Server where
  type ClientOf Server = Client
  endpoint _           = Configurable "server"
  invoke               = invokeServer

-- | 'Endpoint' of the @Server@ node, used by @Haste.App.Simple@.
--   Mainly useful as an argument to 'reconnect'.
simpleEndpoint :: Endpoint
simpleEndpoint = resolveEndpoint $ endpoint (Proxy :: Proxy Server)

-- | Run a simple application in which a single endpoint, 'Server', is run
--   on the server-side. @runSimple@ can still be used with any number of
--   nodes, but all except @Server@ must have their own binaries running
--   somewhere on the network.
runSimpleApp :: Client () -> IO ()
runSimpleApp = runStandaloneApp [endpoint (Proxy :: Proxy Server)]
