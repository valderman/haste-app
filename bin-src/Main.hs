module Main where
import Control.Shell hiding (run, hPutStrLn)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Commands
import Environment
import Options

main = do
  result <- shell $ do
    path <- getEnv "PATH"
    appdir <- withAppDirectory appName pure
    let mypath = case os of
          Windows -> (appdir </> "bin") ++ ";" ++ path
          _       -> (appdir </> "bin") ++ ":" ++ path
    withEnv "PATH" mypath $ run availableCommands cmdline
  case result of
    Left (Failure msg) -> hPutStrLn stderr msg >> exitFailure
    _                  -> return ()
