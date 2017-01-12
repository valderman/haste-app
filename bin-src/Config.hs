module Config
  ( ToolSpec (..), Tool, Config (..)
  , defaultConfig, resolveTool, allTools
  , runTool, requireTools, standardReqs
  , withBuildEnv, cabal, hasteCabal
  ) where
import Control.Shell
import Data.Either
import Data.List (isPrefixOf)
import Data.Maybe
import Data.Version
import Text.ParserCombinators.ReadP

import Environment
import ToolInstallers

data ToolSpec = ToolSpec
  { -- | Name of the tool.
    toolName :: String
    -- | Get path to tool binary, preferring paths in the given list,
    --   if available.
  , toolPath :: [FilePath] -> Shell (Maybe FilePath)
    -- | Download and install tool locally for Haste.App into the given
    --   directory, if possible.
  , toolInstall :: Maybe (Shell ())
  }

type Tool = (String, FilePath)

-- | Per-invocation configuration for the Haste.App build tool.
data Config = Config
  { -- | All external tools available to the build tool.
    tools :: [Tool]
    -- | Any tools that were requested but not found.
  , missingTools :: [ToolSpec]
    -- | Any extra non-option arguments.
  , extraArgs :: [String]
    -- | Download a snapshot of Haste.App from git instead of using the latest
    --   suitable version from Hackage?
    --   Default: @False@
  , useHasteAppGit :: Bool
    -- | Show help message, then exit.
    --   Default: @False@
  , showHelpMessage :: Bool
  }

-- | All external tools needed by the build tool.
allTools :: [ToolSpec]
allTools =
  [ ToolSpec
      { toolName    = "haste-cabal"
      , toolPath    = guessCommand "haste-cabal" anyVersion
      , toolInstall = installHaste
      }
  , ToolSpec
      { toolName    = "cabal"
      , toolPath    = guessCommand "cabal" anyVersion
      , toolInstall = Nothing
      }
  , ToolSpec
      { toolName    = "GHC 7.10"
      , toolPath    = guessCommand "ghc" (versionIs $ makeVersion [7,10])
      , toolInstall = Nothing
      }
  , ToolSpec
      { toolName    = "Haste"
      , toolPath    = guessCommand "hastec" (versionAtLeast hasteVer)
      , toolInstall = installHaste
      }
  ]
  where
    hasteVer = makeVersion [0,5,5,1]

-- | Build the default configuration.
defaultConfig :: [String] -> Shell Config
defaultConfig extras = withAppDirectory appName $ \appdir -> do
  ts <- mapM (resolveTool [appdir </> "bin"]) allTools
  let (missing, present) = partitionEithers ts
  return $ Config
    { tools           = present
    , missingTools    = missing
    , extraArgs       = extras
    , useHasteAppGit  = False
    , showHelpMessage = False
    }

-- | Perform the given action if all tools in the given list are present. Also
--   filter out any tool that was not explicitly requested from the config's
--   list of available tools.
requireTools :: [String] -> (Config -> Shell a) -> Config -> Shell a
requireTools req act cfg = do
    case [t | t <- missingTools cfg, toolName t `elem` req] of
      []      -> act (cfg {tools = [t | t <- tools cfg, fst t `elem` req]})
      missing -> fail $ missingToolsError missing
  where
    missingToolsError ts = concat
      [ "Required external programs were not found:\n"
      , unlines $ ["  " ++ toolName t | t <- ts]
      , case partitionInstallable ts of
          ([], uninstallable) ->
            "Please install these programs before running Haste.App."
          (installable, []) ->
            "Please run `haste-app toolsetup' to install them."
          (installable, uninstallable) -> init $ unlines
            [ "\nThe following programs can be automatically installed:"
            , init $ unlines $ map ("  " ++ ) installable
            , "Please run `haste-app toolsetup' to install them."
            , "\nHowever, the following programs need to be installed manually:"
            , init $ unlines $ map ("  " ++ ) uninstallable
            , "Please install these programs before running Haste.App."
            ]
      ]

-- | Split the list of tool specifications into a list of installable and
--   uninstallable tools.
partitionInstallable :: [ToolSpec] -> ([String], [String])
partitionInstallable ts = ( [toolName t | t <- ts, isJust $ toolInstall t]
                          , [toolName t | t <- ts, isNothing $ toolInstall t])

-- | Require Haste, GHC, haste-cabal and cabal to be present.
standardReqs :: (Config -> Shell a) -> Config -> Shell a
standardReqs = requireTools
  [ "haste-cabal"
  , "cabal"
  , "GHC 7.10"
  , "Haste"
  ]

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
--   the given list of search paths is preferred. The given predicate is
--   executed over each path to verify that it is indeed the correct binary.
--   If no search path contains the binary, the default one on the system path
--   is used.
guessCommand :: String
             -> (FilePath -> Shell Bool)
             -> [FilePath]
             -> Shell (Maybe FilePath)
guessCommand cmd isok paths = go $ (map (</> cmd) paths) ++ [cmd]
  where
    go (p:ps) =
      (guard (isok p) >> pure (Just p)) `orElse` go ps
    go _ =
      pure Nothing

-- | Flags that will produce a numeric version when passed to a tool.
versionFlags :: [String]
versionFlags = ["--version", "--numeric-version"]

-- | Check that the program's version is an exact match as far as specified.
--   For instance, GHC 7.10.3 will match "7.10" but not "7.10.4".
versionIs :: Version -> FilePath -> Shell Bool
versionIs (Version ver _) =
  checkVer ((ver `isPrefixOf`) . versionBranch) versionFlags

-- | Check that the program's version is an exact match as far as specified.
--   For instance, GHC 7.10.3 will match "7.10.4" but not "7.10.4".
versionAtLeast :: Version -> FilePath -> Shell Bool
versionAtLeast ver = checkVer (>= ver) versionFlags

-- | Don't check the version.
anyVersion :: FilePath -> Shell Bool
anyVersion p = (capture2 (run p ["--help"]) >> pure True) `orElse` pure False

-- | Check whether the given predicate matches the output produced by the given
--   program when invoked with any of the given flags.
checkVer :: (Version -> Bool) -> [String] -> FilePath -> Shell Bool
checkVer p (flag:flags) cmd = do
  mver <- readVer <$> capture (run cmd [flag])
  case mver of
    Just ver | p ver -> return True
    _                -> checkVer p flags cmd
checkVer _ _ _ = do
  return False

readVer :: String -> Maybe Version
readVer s =
  case readP_to_S parseVersion s of
    [] -> Nothing
    vs -> Just $ fst $ last vs

-- | Perform the given computation if we're in a Haste.App build environment,
--   otherwise complain and exit. Also ensures that @cabal@ and @haste-cabal@
--   are available.
withBuildEnv :: (Config -> Shell a) -> Config -> Shell a
withBuildEnv act = standardReqs $ \cfg -> do
  hbe <- hasBuildEnv
  if hbe
    then act cfg
    else fail "not a Haste.App build environment; use `haste-app init' to create one first"

-- | Run @haste-cabal@ tool in client sandbox.
hasteCabal :: Config -> [String] -> Shell ()
hasteCabal cfg args = runTool "haste-cabal" cfg args'
  where args' = ("--sandbox-config-file=" ++ clientSandboxConfig) : args

-- | Run @cabal@ tool in server sandbox.
cabal :: Config -> [String] -> Shell ()
cabal cfg args = runTool "cabal" cfg args'
  where args' = ("--sandbox-config-file=" ++ serverSandboxConfig) : args
