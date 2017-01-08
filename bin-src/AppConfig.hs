{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- | Configuration for the application being built.
module AppConfig
  ( ExeType (..), AppConfig (..), Target (..)
  , readAppConfig, hasServerExes, hasClientExes
  ) where
import Control.Applicative
import Control.Shell
import Data.Aeson
import Data.Aeson.Types (typeMismatch, Parser)
import qualified Data.ByteString.Lazy as BS
import Data.Either
import qualified Data.Text as Text
import qualified Data.Vector as Vec

import Environment hiding (AppPart (..))

-- | Build target type.
data ExeType
  = Client
  | Server
  | Both
  | Standalone
    deriving (Show, Eq, Ord)

-- | Application-wide configuration.
data AppConfig = AppConfig
  { -- | All application build targets.
    targets :: [Target]
    -- | Use this executable type for targets where none is specified.
    --   Default: @Both@
  , defaultExeType :: ExeType
    -- | Use this embed directory for targets where none is specified.
    --   Default: @"."@
  , defaultEmbedDir :: FilePath
    -- | Embed these files for targets where none are specified.
    --   Default: @[]@
  , defaultEmbedFiles :: [FilePath]
  } deriving Show

data Target = Target
  { -- | Name of the target executable. Must match an executable in the
    --   project's cabal file.
    exeName :: String
    -- | What type of executable should be built? Should be @Standalone@ if
    --   "Haste.App.Standalone" or "Haste.App.Simple" is used.
    --   Default: 'defaultExeType'
  , exeType :: ExeType
    -- | In which directory do extra files to embed into a standalone executable
    --   reside? Must be relative to project root.
    --   Ignored if 'exeType' is not @Standalone@.
    --   Default: 'defaultEmbedDir'
  , embedDir :: FilePath
    -- | Names of all extra files and directories that should be embedded into a
    --   standalone executable. These files and directories must reside in
    --   'embedDir'.  Ignored if 'exeType' is @Standalone@.
    --   Default: 'defaultEmbedFiles'
  , embedFiles :: [FilePath]
  } deriving Show

-- | The default application configuration, without targets.
defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig
  { targets = []
  , defaultExeType = Both
  , defaultEmbedDir = "."
  , defaultEmbedFiles = []
  }

-- | Target using the defaults from the application configuration for
--   everything except its name.
defaultTarget :: AppConfig -> Target
defaultTarget AppConfig{..} = Target
  { exeName = ""
  , exeType = defaultExeType
  , embedDir = defaultEmbedDir
  , embedFiles = defaultEmbedFiles
  }

instance FromJSON ExeType where
  parseJSON (String "client")     = pure Client
  parseJSON (String "server")     = pure Server
  parseJSON (String "both")       = pure Both
  parseJSON (String "standalone") = pure Standalone
  parseJSON x                     = typeMismatch "ExeType" x

instance ToJSON ExeType where
  toJSON Client     = String "client"
  toJSON Server     = String "server"
  toJSON Both       = String "both"
  toJSON Standalone = String "standalone"

-- | Parse a target, filling in any missing fields with the defaults from the
--   given application config.
parseTarget :: AppConfig -> Value -> Parser Target
parseTarget AppConfig{..} (Object obj) =
    Target <$> obj .:  "name"
           <*> obj .:? "type"        .!= defaultExeType
           <*> obj .:? "embed-dir"   .!= defaultEmbedDir
           <*> obj .:? "embed-files" .!= defaultEmbedFiles
parseTarget conf (String name) =
  return $ (defaultTarget conf) {exeName = Text.unpack name}
parseTarget _ x =
  typeMismatch "Target" x

-- | Parse a list of targets.
parseTargets :: AppConfig -> Value -> Parser [Target]
parseTargets conf (Array ts) = mapM (parseTarget conf) $ Vec.toList ts
parseTargets _ x             = typeMismatch "[Target]" x

instance ToJSON Target where
  toJSON Target{..} = object
    [ "name"       .= toJSON exeName
    , "type"       .= toJSON exeType
    , "embed-dir"  .= toJSON embedDir
    , "embedFiles" .= toJSON embedFiles
    ]

instance FromJSON AppConfig where
  parseJSON (Object obj)  = do
    cfg <-  AppConfig <$> pure []
                      <*> obj .:? "type"        .!= Both
                      <*> obj .:? "embed-dir"   .!= "."
                      <*> obj .:? "embed-files" .!= []
    mname <- obj .:? "name"
    mtarr <- obj .:? "targets"
    case (mname, mtarr) of
      (Just name, Nothing) -> do
        return $ cfg {targets = [(defaultTarget cfg) {exeName = name}]}
      (Nothing, Just tarr) -> do
        ts <- parseTargets cfg tarr
        return $ cfg {targets = ts}
      (Nothing, Nothing) -> do
        fail "app config must have either `targets' or `name' on top level"
      _ -> do
        fail "app config can't have both `targets' and `name' on top level"
  parseJSON arr@(Array _) = do
    ts <- parseTargets defaultAppConfig arr
    return $ defaultAppConfig {targets = ts}
  parseJSON x = typeMismatch "AppConfig" x

instance ToJSON AppConfig where
  toJSON AppConfig{..} = object
    [ "targets"     .=  targets
    , "type"        .= defaultExeType
    , "embed-dir"   .= defaultEmbedDir
    , "embed-files" .= defaultEmbedFiles
    ]

-- | Read the application config file, if available.
--   Fails if the config could not be parsed, and returns @Nothing@ if there
--   was no config file.
readAppConfig :: Shell (Maybe AppConfig)
readAppConfig = do
  eres <- try $ liftIO $ BS.readFile appConfigFile
  case eres of
    Right x -> either failAppConfBroken (pure . Just) $ eitherDecode' x
    _       -> return Nothing

-- | Does the application contain any executables that need to be built for the
--   client?
hasClientExes :: AppConfig -> Bool
hasClientExes = any (\t -> exeType t /= Server) . targets

-- | Does the application contain any executables that need to be built for a
--   server?
hasServerExes :: AppConfig -> Bool
hasServerExes = any (\t -> exeType t /= Client) . targets

-- | Fail with the given parse error due to a broken application config.
failAppConfBroken :: String -> Shell a
failAppConfBroken err = fail $ concat
  [ "unable to parse ", appConfigFile, ": "
  , err
  ]
