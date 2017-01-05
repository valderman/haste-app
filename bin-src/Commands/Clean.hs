-- | Clean out build files for application.
module Commands.Clean (clean) where
import Control.Shell

import Config
import Environment

clean :: Config -> Shell ()
clean cfg = do
  echo "cleaning..."
  capture $ cabal cfg ["clean", buildDir Server]
  capture $ hasteCabal cfg ["clean", buildDir Client]
  return ()
