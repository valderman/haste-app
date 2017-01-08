-- | Clean out build files for application.
module Commands.Clean (clean) where
import Control.Shell

import Config
import Environment

clean :: Config -> Shell ()
clean = withBuildEnv $ \cfg -> do
  echo "cleaning..."
  capture2 $ cabal cfg ["clean", buildDir Server]
  capture2 $ hasteCabal cfg ["clean", buildDir Client]
  return ()
