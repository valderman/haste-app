module Config
  ( ToolSpec (..), Tool, Config (..)
  , defaultConfig, resolveTool, allTools
  , runTool
  ) where
import Control.Shell
import Data.Either

-- | Name of the Haste.App build tool.
appName :: String
appName = "haste-app"

data ToolSpec = ToolSpec
  { -- | Name of the tool.
    toolName :: String
    -- | Get path to tool binary, preferring paths in the given list,
    --   if available.
  , toolPath :: [FilePath] -> Shell (Maybe FilePath)
    -- | Download and install tool locally for Haste.App into the given
    --   directory, if possible.
  , toolInstall :: Maybe (FilePath -> Shell ())
  }

type Tool = (String, FilePath)

-- | Per-invocation configuration for the Haste.App build tool.
data Config = Config
  { -- | All external tools available to the build tool.
    tools     :: [Tool]
    -- | Any extra non-option arguments.
  , extraArgs :: [String]
  }

-- | All external tools needed by the build tool.
allTools :: [ToolSpec]
allTools =
  [ ToolSpec
      { toolName    = "haste-cabal"
      , toolPath    = guessCommand "haste-cabal"
      , toolInstall = Nothing
      }
  , ToolSpec
      { toolName    = "cabal"
      , toolPath    = guessCommand "cabal"
      , toolInstall = Nothing
      }
  ]

-- | Build the default configuration.
defaultConfig :: [String] -> Shell Config
defaultConfig extras = withAppDirectory appName $ \appdir -> do
  ts <- mapM (resolveTool [appdir </> "bin"]) allTools
  return $ Config
    { tools     = rights ts
    , extraArgs = extras
    }

-- | Attempt to locate the given tool and return a @(name, binary)@ pair if
--   found. If not, return the tool specification itself.
resolveTool :: [FilePath] -> ToolSpec -> Shell (Either ToolSpec Tool)
resolveTool paths tool = do
  mpath <- toolPath tool paths
  case mpath of
    Just path -> return $ Right (toolName tool, path)
    _         -> return $ Left tool

-- | Look up the tool with the given name in the given config, and run it.
--   Fails if the tool is not found.
runTool :: String -> Config -> [String] -> Shell ()
runTool name cfg
  | Just path <- lookup name (tools cfg) = run path
  | otherwise                            = fail $ "[BUG] no such tool: " ++ name

-- | Try to guess the path of the given command. The first working binary in
--   the given list of search paths is preferred. Binaries are executed without
--   any argument to determine whether they are "working" or not.
--   If no search path contains the binary, the default one on the system path
--   is used.
guessCommand :: String -> [FilePath] -> Shell (Maybe FilePath)
guessCommand cmd paths = go $ (map (</> cmd) paths) ++ [cmd]
  where
    go (p:ps) = (capture2 (run p []) >> pure (Just p)) `orElse` go ps
    go _      = pure Nothing
