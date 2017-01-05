-- | Configure the client and server parts.
module Commands.Configure (configure) where
import Control.Shell

import Config
import Environment

configure :: Config -> Shell ()
configure = withBuildEnv $ \cfg -> do
    cabal cfg $ withCabalFlags ["configure"]
    hasteCabal cfg $ withCabalFlags ["configure"]
