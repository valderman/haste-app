-- | Initialize the Haste.App build environment.
module Commands.Init (initialize) where
import Control.Shell

import Config
import Environment

initialize :: Config -> Shell ()
initialize cfg = do
  cabal cfg ["sandbox", "init"]
  hasteCabal cfg ["sandbox", "init"]
