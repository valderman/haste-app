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
        capture2 $ runTool "cabal" cfg ["sandbox", "init"]
      inDirectory (scratchDir Client) $ do
        capture2 $ runTool "haste-cabal" cfg ["sandbox", "init"]

      -- TODO: remove when Haste 0.6 is released
      installLibs (useHasteAppGit cfg)
      mapM_ (addSandboxSource cfg) ["haste-prim", "haste-lib"]
      when (useHasteAppGit cfg) $ addSandboxSource cfg "haste-app"
      echo "all done; now run `haste-app setup' to install any dependencies"

-- | Att a package downloaded from off-hackage as a sandbox package source.
addSandboxSource :: Config -> String -> Shell ()
addSandboxSource cfg pkgname = do
    cabal cfg ["sandbox", "add-source", dir]
    hasteCabal cfg ["sandbox", "add-source", dir]
  where
    dir = packagesDir </> pkgname

-- | Fetch snapshots of recent haste libraries. Also fetch haste-app snapshot
--   if explicitly requested by the user.
installLibs :: Bool -> Shell ()
installLibs hasteAppGit = do
  when (isDirectory packagesDir) $ rmdir packagesDir
  mkdir True packagesDir
  when hasteAppGit $ do
    fetchLib "https://github.com/valderman/haste-app/archive/master.zip"
  fetchLib "https://github.com/valderman/haste-compiler/archive/master.zip"
  inDirectory packagesDir $ do
    when hasteAppGit $ mv "haste-app-master" "haste-app"
    mv ("haste-compiler-master" </> "libraries" </> "haste-prim") "haste-prim"
    mv ("haste-compiler-master" </> "libraries" </> "haste-lib") "haste-lib"
    rmdir "haste-compiler-master"

-- | Fetch haste-lib and haste-prim snapshots.
fetchLib :: URI -> Shell ()
fetchLib uri = do
  inDirectory packagesDir $ do
    fetchFile (takeFileName uri) uri
    extractWith (defaultExtractOptions { removeArchive = True
                                       , separateDirectory = False})
                (takeFileName uri)
