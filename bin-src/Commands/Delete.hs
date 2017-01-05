-- | Delete the Haste.App build environment.
module Commands.Delete (delete) where
import Control.Shell

import Config
import Environment

delete :: Config -> Shell ()
delete _ = do
  be <- hasBuildEnv
  if be
    then rmdir scratchRoot
    else echo "not a Haste.App build environment"
