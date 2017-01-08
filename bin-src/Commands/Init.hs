-- | Initialize the Haste.App build environment.
module Commands.Init (initialize) where
import Control.Shell
import Control.Shell.Download
import Control.Shell.Extract

import Config
import Environment

initialize :: Config -> Shell ()
initialize = standardReqs $ \cfg -> do
  hbe <- hasBuildEnv
  if hbe
    then fail "build environment already exists"
    else do
      echo "initializing environment..."
      mkdir True (scratchDir Server)
      mkdir True (scratchDir Client)
      inDirectory (scratchDir Server) $ do
        capture2 $ run "cabal" ["sandbox", "init"]
      inDirectory (scratchDir Client) $ do
        capture2 $ run "haste-cabal" ["sandbox", "init"]

      -- TODO: remove when Haste 0.6 and Haste.App are released
      installLibs
      mapM_ (addSandboxSource cfg) ["haste-prim", "haste-lib", "haste-app"]
      echo "all done; now run `haste-app setup' to install any dependencies"

addSandboxSource :: Config -> String -> Shell ()
addSandboxSource cfg pkgname = do
    cabal cfg ["sandbox", "add-source", dir]
    hasteCabal cfg ["sandbox", "add-source", dir]
  where
    dir = packagesDir </> pkgname

installLibs :: Shell ()
installLibs = do
  when (isDirectory packagesDir) $ rmdir packagesDir
  mkdir True packagesDir
  fetchLib "http://haste-lang.org/haste-lib-0.6-snapshot.tar.bz2"
  fetchLib "https://github.com/valderman/haste-app/archive/master.zip"
  inDirectory packagesDir $ mv "haste-app-master" "haste-app"

-- | Fetch haste-lib and haste-prim snapshots.
fetchLib :: URI -> Shell ()
fetchLib uri = do
  inDirectory packagesDir $ do
    bs <- fetchFile (takeFileName uri) uri
    extractWith (defaultExtractOptions { removeArchive = True
                                       , separateDirectory = False})
                (takeFileName uri)
