-- | Utilities for working with Haste.App's multiple environments.
module Environment where
import Control.Shell

import Config

type TargetName = String

noTarget :: TargetName
noTarget = ""

-- | Write which log file?
data Stage = Setup | Install | Configure | Build TargetName
  deriving (Show, Eq)

-- | Name of the given stage, for the given part. For use in file names, etc.
stagePartName :: Stage -> AppPart -> String
stagePartName s p = stageName s ++ "-" ++ appPartName p

-- | Names for stages, for use in file names and similar.
stageName :: Stage -> String
stageName Setup      = "setup"
stageName Install    = "install"
stageName Configure  = "configure"
stageName (Build "") = "build"
stageName (Build t)  = "build-" ++ t

data AppPart = Client | Server
  deriving (Show, Read, Eq)

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

-- | Relative path to log directory.
logDir :: FilePath
logDir = "_logs"

-- | Relative path to the given log file.
logFilePath :: Stage -> AppPart -> FilePath
logFilePath s p = logDir </> logFileName s p

-- | File name of the given log file, without leading path.
logFileName :: Stage -> AppPart -> FilePath
logFileName s p = stagePartName s p <.> "log"

-- | Directory for old log files.
oldLogDir :: FilePath
oldLogDir = logDir </> "_old"

-- | Back up any existing log files.
backupLogFiles :: Shell ()
backupLogFiles = do
    unless (isDirectory oldLogDir) $ mkdir True oldLogDir
    fs <- filterM isLogFile =<< ls logDir
    forM_ fs $ \f -> do
      mv f (oldLogDir </> takeFileName f)
  where
    isLogFile f
      | takeExtension f == ".log" = isFile f
      | otherwise                 = pure False

-- | Log standard output and error to the given files.
withLogging :: Stage -> AppPart -> Shell () -> Shell ()
withLogging st p act = do
  unless (isDirectory logDir) $ mkdir True logDir
  backupLogFiles
  (out, err, reason) <- capture3 act
  writeLogFile st p out err
  case reason of
    Failure msg -> failStage msg err
    _           -> return ()
  where
    failStage err msg = fail $ init $ concat
      [ "stage ", stagePartName st p, " failed (", err, ")\n\n"
      , "relevant cabal output:\n"
      ] ++ unlines (map ("  " ++) (lines msg))

-- | Write the specified log to file.
writeLogFile :: Stage -> AppPart -> String -> String -> Shell ()
writeLogFile st p out err = do
  output (logFilePath st p) $ unlines
    [ "======================="
    , "= Errors and warnings ="
    , "======================="
    , err
    , ""
    , "======================="
    , "= Diagnostic messages ="
    , "======================="
    , out
    ]

-- | Name of application configuration JSON file.
appConfigFile :: FilePath
appConfigFile = "haste-app.json"

-- | Fail with the given parse error due to a broken application config.
failAppConfBroken :: String -> Shell a
failAppConfBroken err = fail $ concat
  [ "unable to parse ", appConfigFile, ": "
  , err
  ]
