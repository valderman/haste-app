-- | Configure the client and server parts.
module Commands.Configure (configure) where
import Control.Shell

import AppConfig
import Config
import Environment

configure :: Config -> Shell AppConfStatus
configure = withBuildEnv $ \cfg -> do
  cfgstat <- readAppConfig
  (conf_server, conf_client) <- case cfgstat of
    AppConfOK cfg   -> return (hasServerExes cfg, hasClientExes cfg)
    AppConfMissing  -> return (True, True)
    AppConfBroken e -> failAppConfBroken e
  when conf_server $ cabal cfg $ withCabalFlags ["configure"]
  when conf_client $ hasteCabal cfg $ withCabalFlags ["configure"]
  return cfgstat
