-- | Utilities for working with Haste.App's multiple environments.
module Environment
  ( TargetName, AppPart (..), OS (..)
  , os, hasBuildEnv
  , scratchRoot, scratchDir, buildDir, artifactDir, packagesDir, appName
  , appConfigFile, appPartName, noTarget
  , withCabalFlags
  , clientSandboxConfig, serverSandboxConfig
  ) where
import Control.Shell
import qualified System.Info (os)

type TargetName = String

noTarget :: TargetName
noTarget = ""

data OS = Linux | Windows | MacOS
  deriving (Show, Eq, Ord)

data AppPart = Client | Server
  deriving (Show, Eq, Ord)

-- | What OS are we running on?
os :: OS
os | System.Info.os == "mingw32" = Windows
   | System.Info.os == "linux"   = Linux
   | System.Info.os == "darwin"  = MacOS
   | otherwise                   = error "unsupported operating system"

-- | Name of the Haste.App build tool.
appName :: String
appName = "haste-app"

-- | Name of the given AppPart, for use in file names and similar.
appPartName :: AppPart -> String
appPartName Client = "client"
appPartName Server = "server"

-- | Relative path to the scratch directory for each part.
scratchDir :: AppPart -> FilePath
scratchDir p = scratchRoot </> appPartName p

-- | Root scratch directory.
scratchRoot :: FilePath
scratchRoot = ".haste-app-env"

-- | Directory in which to store local copies of packages.
packagesDir :: FilePath
packagesDir = scratchRoot </> "packages"

-- | Does the current working directory have a build environment?
hasBuildEnv :: Shell Bool
hasBuildEnv = isDirectory scratchRoot

-- | Name of the cabal sandbox configuration file.
sandboxConfigFile :: FilePath
sandboxConfigFile = "cabal.sandbox.config"

-- | Path to server sandbox config file.
serverSandboxConfig :: FilePath
serverSandboxConfig = scratchDir Server </> sandboxConfigFile

-- | Path to client sandbox config file.
clientSandboxConfig :: FilePath
clientSandboxConfig = scratchDir Client </> sandboxConfigFile

-- | Add @-fhaste-app@ list of args.
withCabalFlags :: [String] -> [String]
withCabalFlags = ("-fhaste-app" :)

-- | Cabal @--builddir@ argument for the given application part.
buildDir :: AppPart -> String
buildDir part = "--builddir=" ++ (scratchDir part </> "dist")

-- | Relative path to directory in which to place build artifacts.
artifactDir :: FilePath
artifactDir = "_app"

-- | Name of application configuration JSON file.
appConfigFile :: FilePath
appConfigFile = "haste-app.json"
