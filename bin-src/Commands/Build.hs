{-# LANGUAGE RecordWildCards #-}
-- | Building all parts of a Haste.App application.
module Commands.Build (build) where
import Control.Shell

import AppConfig hiding (ExeType (..))
import qualified AppConfig as App (ExeType (..))
import Commands.Configure
import Config
import Environment
import Haste.App.Standalone.Embed
import Logging

build :: Config -> Shell ()
build = withBuildEnv $ \cfg -> do
    -- (Re-)create artifact dir
    when (isDirectory artifactDir) $ rmdir artifactDir
    mkdir True artifactDir

    -- Configure and build
    mapp <- configure cfg
    case mapp of
      Just app -> buildApp cfg app
      Nothing  -> buildWithoutAppConfig cfg

-- | Build an application from a config file.
buildApp :: Config -> AppConfig -> Shell ()
buildApp cfg AppConfig{..} = do
    mapM_ buildTarget targets
  where
    buildTarget Target{..} = do
      -- Build client and server parts
      unless (exeType == App.Client) $ withLogging (Build exeName) Server $ do
        cabal cfg ["build", exeName, buildDir Server]
      unless (exeType == App.Server) $ withLogging (Build exeName) Client $ do
        hasteCabal cfg ["build", exeName, buildDir Client]

      -- Finalize build and copy artifacts
      case exeType of
        App.Standalone -> do
          embedAndCopy exeName exeName embedDir embedFiles
        App.Client -> do
          copyArtifact Client exeName
        App.Server -> do
          copyArtifact Server exeName
        App.Both -> do
          copyArtifact Client exeName
          copyArtifact Server exeName

-- | Build application from cabal file only.
buildWithoutAppConfig :: Config -> Shell ()
buildWithoutAppConfig cfg = do
    configure cfg
    buildServer cfg
    buildClient cfg

    client_bas <- findBuildArtifacts Client
    server_bas <- findBuildArtifacts Server

    case (client_bas, server_bas) of
      ([cli], [srv]) -> do
        ver <- words <$> capture (run srv ["--haste-app-version"])
        case ver of
          ["Haste.App,", _, _] -> embedAndCopy cli srv "." []
          _                    -> copyAllArtifacts client_bas server_bas
      _ -> do
        copyAllArtifacts client_bas server_bas
  where
    buildServer cfg = do
      withLogging (Build noTarget) Server $ do
        cabal cfg ["build", buildDir Server]

    buildClient cfg = do
      withLogging (Build noTarget) Client $ do
        hasteCabal cfg ["build", buildDir Client]

-- | Copy the given artifact into the build artifact directory.
copyArtifact :: AppPart -> TargetName -> Shell ()
copyArtifact part art = cp from to
  where
    from = buildArtifactPath part art
    to = case part of
           Server -> artifactDir </> takeFileName from
           Client -> replaceExtension (artifactDir </> takeFileName from) ".js"

-- | Copy all given artifacts into the build artifact directory.
copyAllArtifacts :: [TargetName] -> [TargetName] -> Shell ()
copyAllArtifacts client_bas server_bas = do
  mapM_ (copyArtifact Client) client_bas
  mapM_ (copyArtifact Server) server_bas

-- | Copy server binary to artifact directory, then embed client program and any
--   extra files.
embedAndCopy :: TargetName -> TargetName -> FilePath -> [FilePath] -> Shell ()
embedAndCopy cli srv embedDir embedFiles = do
    echo "embedding client and extra files..."
    copyArtifact Server srv
    withCustomTempFile TextMode "." $ \js h -> do
      -- Use a temp file name to avoid putting build paths into the artifact.
      hClose h
      cp cliFile js
      liftIO $ embedFilesInto 1 srvFile js extras
  where
    extras  = [embedDir </> f | f <- embedFiles]
    cliFile = buildArtifactPath Client cli
    srvFile = artifactDir </> buildArtifactName Server srv

-- | Get all build artifacts for the given part.
findBuildArtifacts :: AppPart -> Shell [TargetName]
findBuildArtifacts part = do
    dirs <- filter (/= "autogen") <$> ls build
    exes <- zipWith getExe dirs <$> sequence [ls (build </> d) | d <- dirs]
    return $ filter (not . null) exes
  where
    build = scratchDir part </> "dist" </> "build"
    getExe dir files =
      case filter (\f -> takeBaseName f == dir) files of
        []    -> ""
        (f:_) -> takeBaseName f

-- | Get the path of the build artifact with the given name.
buildArtifactPath :: AppPart -> TargetName -> FilePath
buildArtifactPath part name =
    scratchDir part </> "dist" </> "build" </> name </> exe
  where
    exe | os == Windows = name <.> "exe"
        | otherwise     = name

-- | Get the file name of the build artifact with the given name.
buildArtifactName :: AppPart -> TargetName -> FilePath
buildArtifactName Client =
  flip replaceExtension ".js" . buildArtifactPath Client
buildArtifactName Server =
  takeFileName . buildArtifactPath Server
