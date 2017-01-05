-- | Utilities for working with Haste.App's multiple environments.
module Environment where
import Control.Shell
import Config

data AppPart = Client | Server
  deriving (Show, Read, Eq)

-- | Relative path to the file determining which application part is active.
activePartFile :: FilePath
activePartFile = ".active"

-- | Relative path to the scratch directory for each part.
scratchDir :: AppPart -> FilePath
scratchDir Client = ".client"
scratchDir Server = ".server"

-- | Directory of the currently active sandbox.
sandboxDir :: AppPart -> FilePath
sandboxDir part = scratchDir part </> "cabal-sandbox"

-- | Name of the cabal sandbox configuration file.
sandboxConfigFile :: FilePath
sandboxConfigFile = "cabal.sandbox.config"

-- | Path to server sandbox config file.
serverSandboxConfig :: FilePath
serverSandboxConfig = scratchDir Server </> sandboxConfigFile

-- | Path to client sandbox config file.
clientSandboxConfig :: FilePath
clientSandboxConfig = scratchDir Client </> sandboxConfigFile

-- | Run @haste-cabal@ tool in client sandbox.
hasteCabal :: Config -> [String] -> Shell ()
hasteCabal cfg args = runTool "haste-cabal" cfg args'
  where args' = ("--sandbox-config-file=" ++ clientSandboxConfig) : args

-- | Run @cabal@ tool in server sandbox.
cabal :: Config -> [String] -> Shell ()
cabal cfg args = runTool "cabal" cfg args'
  where args' = ("--sandbox-config-file=" ++ serverSandboxConfig) : args

-- | Cabal @--builddir@ argument for the given application part.
buildDir :: AppPart -> String
buildDir part = "--builddir=" ++ (scratchDir part </> "dist")

-- | Relative path to directory in which to place build artifacts.
artifactDir :: FilePath
artifactDir = "_app"
