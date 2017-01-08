module Logging (Stage (..), withLogging) where
import Control.Shell

import Environment

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
  echo $ stageReport st p
  (out, err, reason) <- capture3 act
  writeLogFile st p out err
  case reason of
    Failure msg -> failStage msg err
    _           -> return ()
  where
    failStage err msg = fail $ init $ concat
      [ "\nstage ", stagePartName st p, " failed:\n"
      , "  ", err, "\n\n"
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

-- | Progress report line for the given stage and part.
stageReport :: Stage -> AppPart -> String
stageReport (Build "") p =
  concat ["building ", appPartName p, "(s)..."]
stageReport (Build t) p =
  concat ["building ", t, "-", appPartName p, "..."]
stageReport Configure p =
  concat ["configuring ", appPartName p, "(s)..."]
stageReport Install p =
  concat ["installing dependencies for ", appPartName p, "(s)..."]
stageReport Setup p =
  concat ["initializing build environment for ", appPartName p, "(s)..."]
