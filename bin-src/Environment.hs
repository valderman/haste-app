-- | Utilities for working with Haste.App's multiple environments.
module Environment where
import Control.Shell
import Config

data AppPart = Client | Server
  deriving (Show, Read, Eq)

-- | Relative path to the scratch directory for each part.
scratchDir :: AppPart -> FilePath
scratchDir Client = scratchRoot </> "client"
scratchDir Server = scratchRoot </> "server"

-- | Root scratch directory.
scratchRoot :: FilePath
scratchRoot = ".haste-app-env"

-- | Does the current working directory have a build environment?
hasBuildEnv :: Shell Bool
hasBuildEnv = isDirectory scratchRoot

-- | Perform the given computation if we're in a Haste.App build environment,
--   otherwise complain and exit. Also ensures that @cabal@ and @haste-cabal@
--   are available.
withBuildEnv :: (Config -> Shell a) -> Config -> Shell a
withBuildEnv act = standardReqs $ \cfg -> do
  hbe <- hasBuildEnv
  if hbe
    then act cfg
    else fail "not a Haste.App build environment; use `haste-app init' to create one first"

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

-- | Add @-fhaste-app@ list of args.
withCabalFlags :: [String] -> [String]
withCabalFlags = ("-fhaste-app" :)

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

-- | Name of application configuration JSON file.
appConfigFile :: FilePath
appConfigFile = "haste-app.json"

-- | Fail with the given parse error due to a broken application config.
failAppConfBroken :: String -> Shell a
failAppConfBroken err = fail $ concat
  [ "unable to parse ", appConfigFile, ": "
  , err
  ]
