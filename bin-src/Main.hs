module Main where
import Control.Shell (shell, cmdline, ExitReason (..))
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Options
import Commands

main = do
  result <- shell $ run availableCommands cmdline
  case result of
    Left (Failure msg) -> hPutStrLn stderr msg >> exitFailure
    _                  -> return ()
