{-# LANGUAGE RecordWildCards #-}
-- | Building all parts of a Haste.App application.
module Commands.Build (build) where
import Control.Shell
import System.Info (os)

import AppConfig hiding (ExeType (..))
import qualified AppConfig as App (ExeType (..))
import Config
import Environment
import Commands.Configure

type TargetName = String

build :: Config -> Shell ()
build = withBuildEnv $ \cfg -> do
    -- (Re-)create artifact dir
    when (isDirectory artifactDir) $ rmdir artifactDir
    mkdir True artifactDir

    -- Configure and build
    mapp <- configure cfg
    case mapp of
      Just app -> buildApp cfg app
      _        -> buildWithoutAppConfig cfg

-- | Build an application from a config file.
buildApp :: Config -> AppConfig -> Shell ()
buildApp cfg AppConfig{..} = do
    echo ""
    echo "========================================"
    echo "= Building targets from haste-app.json ="
    echo "========================================"
    echo ""
    mapM_ buildTarget targets
  where
    buildTarget Target{..} = do
      -- Build client and server parts
      unless (exeType == App.Client) $ do
        cabal cfg ["build", exeName, buildDir Server]
      unless (exeType == App.Server) $ do
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

    reportArtifacts $ concat
      [ map (buildArtifactName Client) client_bas
      , map (buildArtifactName Server) server_bas
      ]
  where
    buildServer cfg = do
      echo ""
      echo "======================"
      echo "= Building server(s) ="
      echo "======================"
      echo ""
      cabal cfg ["build", buildDir Server]

    buildClient cfg = do
      echo ""
      echo "======================"
      echo "=  Building  client  ="
      echo "======================"
      echo ""
      hasteCabal cfg ["build", buildDir Client]

-- | Copy the given artifact into the build artifact directory.
copyArtifact :: AppPart -> TargetName -> Shell ()
copyArtifact part art = cp from to
  where from = buildArtifactPath part art
        to = artifactDir </> takeFileName from

-- | Copy all given artifacts into the build artifact directory.
copyAllArtifacts :: [TargetName] -> [TargetName] -> Shell ()
copyAllArtifacts client_bas server_bas = do
  mapM_ (copyArtifact Client) client_bas
  mapM_ (copyArtifact Server) server_bas

reportArtifacts :: [FilePath] -> Shell ()
reportArtifacts arts = do
  echo ""
  echo "============="
  echo "=   Done!   ="
  echo "============="
  echo ""
  echo "The following artifacts were built:"
  mapM_ (echo . ("  " ++)) arts

-- | Copy server binary to artifact directory, then embed client program and any
--   extra files.
embedAndCopy :: TargetName -> TargetName -> FilePath -> [FilePath] -> Shell ()
embedAndCopy cli srv embedDir embedFiles = do
    echo ""
    echo "================================"
    echo "= Embedding client into server ="
    echo "================================"
    echo ""
    copyArtifact Server srv
    run srvFile $ ["-s1", "-e", "." </> cliFile] ++ extras
  where
    extras  = map (embedDir </>) embedFiles
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
    exe | os == "mingw32" = name <.> "exe"
        | otherwise       = name

-- | Get the file name of the build artifact with the given name.
buildArtifactName :: AppPart -> TargetName -> FilePath
buildArtifactName Client =
  flip replaceExtension ".js" . buildArtifactPath Client
buildArtifactName Server =
  takeFileName . buildArtifactPath Server
