-- | Configure the client and server parts.
module Commands.Configure (configure) where
import Control.Shell

import AppConfig
import Config
import Environment

configure :: Config -> Shell (Maybe AppConfig)
configure = withBuildEnv $ \cfg -> do
  mapp <- readAppConfig
  when (maybe True hasServerExes mapp) $ do
    cabal cfg $ withCabalFlags ["configure"]
  when (maybe True hasClientExes mapp) $ do
    hasteCabal cfg $ withCabalFlags ["configure"]
  return mapp
