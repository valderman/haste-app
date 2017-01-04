module Main where
import Control.Shell (shell, cmdline, ExitReason (..))
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Options

main = do
  result <- shell $ run [] cmdline
  case result of
    Left (Failure msg) -> hPutStrLn stderr msg >> exitFailure
    _                  -> return ()
