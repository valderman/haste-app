{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- | Configuration for the application being built.
module AppConfig where
import Control.Applicative
import Control.Shell
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Lazy as BS

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
  } deriving Show

data Target = Target
  { -- | Name of the target executable. Must match an executable in the
    --   project's cabal file.
    exeName :: String
    -- | What type of executable should be built? Should be @Standalone@ if
    --   "Haste.App.Standalone" or "Haste.App.Simple" is used.
    --   Default: @Both@
  , exeType :: ExeType
    -- | In which directory do extra files to embed into a standalone executable
    --   reside? Must be relative to project root.
    --   Ignored if 'exeType' is not @Standalone@.
    --   Default: @"."@
  , embedDir :: FilePath
    -- | Names of all extra files and directories that should be embedded into a
    --   standalone executable. These files and directories must reside in
    --   'embedDir'.  Ignored if 'exeType' is @Standalone@.
    --   Default: []
  , embedFiles :: [FilePath]
  } deriving Show

instance FromJSON ExeType where
  parseJSON (String "client")     = pure Client
  parseJSON (String "server")     = pure Server
  parseJSON (String "both")       = pure Both
  parseJSON (String "standalone") = pure Standalone

instance ToJSON ExeType where
  toJSON Client     = String "client"
  toJSON Server     = String "server"
  toJSON Both       = String "both"
  toJSON Standalone = String "standalone"

instance FromJSON Target where
  parseJSON (Object obj) =
    Target <$> obj .:  "name"
           <*> obj .:? "type"        .!= Both
           <*> obj .:? "embed-dir"   .!= "."
           <*> obj .:? "embed-files" .!= []
  parseJSON x = typeMismatch "Target" x

instance ToJSON Target where
  toJSON Target{..} = object
    [ "name"       .= toJSON exeName
    , "type"       .= toJSON exeType
    , "embed-dir"  .= toJSON embedDir
    , "embedFiles" .= toJSON embedFiles
    ]

instance FromJSON AppConfig where
  parseJSON (Object obj)  = AppConfig <$> obj .: "targets"
  parseJSON arr@(Array _) = AppConfig <$> parseJSON arr
  parseJSON x = typeMismatch "AppConfig" x

instance ToJSON AppConfig where
  toJSON AppConfig{..} = object
    [ "targets" .= toJSON targets
    ]

-- | Read the application config file, if available.
readAppConfig :: Shell (Maybe AppConfig)
readAppConfig = do
  eres <- try $ liftIO $ BS.readFile appConfigFile
  case eres of
    Right x -> return $ decode' x
    _       -> return Nothing

-- | Does the application contain any executables that need to be built for the
--   client?
hasClientExes :: AppConfig -> Bool
hasClientExes = any (\t -> exeType t /= Server) . targets

-- | Does the application contain any executables that need to be built for a
--   server?
hasServerExes :: AppConfig -> Bool
hasServerExes = any (\t -> exeType t /= Client) . targets
