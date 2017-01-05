-- | Install a package into the Haste.App build environment.
module Commands.Install (install) where
import Control.Shell

import Config
import Environment

install :: Config -> Shell ()
install = withBuildEnv $ \cfg -> do
    cabal cfg ("install" : extraArgs cfg)
    hasteCabal cfg ("install" : extraArgs cfg)
