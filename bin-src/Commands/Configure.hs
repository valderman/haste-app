-- | Configure the client and server parts.
module Commands.Configure (configure) where
import Control.Shell

import AppConfig hiding (ExeType (..))
import Config
import Environment
import Logging

configure :: Config -> Shell AppConfStatus
configure = withBuildEnv $ \cfg -> do
  cfgstat <- readAppConfig
  (conf_server, conf_client) <- case cfgstat of
    AppConfOK cfg   -> return (hasServerExes cfg, hasClientExes cfg)
    AppConfMissing  -> return (True, True)
    AppConfBroken e -> failAppConfBroken e
  when conf_server $ do
    withLogging Configure Server $ cabal cfg $ withCabalFlags ["configure"]
  when conf_client $ do
    withLogging Configure Client $ hasteCabal cfg $ withCabalFlags ["configure"]
  return cfgstat
