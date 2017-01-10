-- | Install any missing tools.
module Commands.ToolSetup (toolSetup) where
import Control.Shell
import Control.Shell.Download
import Data.Either

import Config
import Environment

toolSetup :: Config -> Shell ()
toolSetup cfg = withAppDirectory appName $ \appdir -> do
  let bindir = appdir </> "bin"
  unless (isDirectory bindir) $ mkdir True bindir
  ts <- forM (missingTools cfg) $ \t -> do
    case toolInstall t of
      Just doInstall -> do
        echo $ "installing " ++ toolName t
        doInstall
        pure $ Right t
      _ -> do
        pure $ Left t
  case lefts ts of
    []  -> echo "done"
    ts' -> echo $ unlines
      [ "the following tools could not be automatically installed:"
      , unlines ["  " ++ toolName t | t <- ts']
      , "please make sure that these tools are installed and present on your "
      , "search path"
      ]
