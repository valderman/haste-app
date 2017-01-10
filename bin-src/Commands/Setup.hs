-- | Set upp all dependencies for Haste.App as well as the local project.
module Commands.Setup (setup) where
import Control.Shell
import Control.Shell.Concurrent

import Config
import Environment
import Logging

setup :: Config -> Shell ()
setup = withBuildEnv $ \cfg -> do
  echo "updating package lists..."
  parallel_
    [ capture2 $ cabal cfg ["update"]
    , capture2 $ hasteCabal cfg ["update"]
    ]
  withLogging Install Server $ do
    cabal cfg $ withCabalFlags [ "install"
                               , "--only-dependencies"
                               , "--constraint=haste-app +with-library"
                               ]
  withLogging Install Client $ do
    hasteCabal cfg $ withCabalFlags [ "install"
                                    , "--only-dependencies"
                                    , "--constraint=haste-app +haste +with-library"
                                    , "--constraint=haste-lib +haste"
                                    , "--constraint=haste-prim +haste"
                                    ]
