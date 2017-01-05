-- | Configure the client and server parts.
module Commands.Configure (configure) where
import Control.Shell

import Config
import Environment

configure :: Config -> Shell ()
configure cfg = withBuildEnv $ do
    cabal cfg ["configure"]
    hasteCabal cfg ["configure"]
