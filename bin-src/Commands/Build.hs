-- | Building all parts of a Haste.App application.
module Commands.Build (build) where
import Control.Shell

import Config
import Environment

build :: Config -> Shell ()
build cfg = do
    buildServer
    buildClient

    when (isDirectory artifactDir) $ rmdir artifactDir
    mkdir True artifactDir

    client_bas <- findBuildArtifacts Client
    server_bas <- findBuildArtifacts Server

    arts <- case (client_bas, server_bas) of
      ([cli], [srv]) -> do
        ver <- words <$> capture (run srv ["--haste-app-version"])
        case ver of
          ["Haste.App,", _, _] -> embedAndCopy cli srv
          _                    -> copyAllArtifacts client_bas server_bas
      _ -> do
        copyAllArtifacts client_bas server_bas

    reportArtifacts arts
  where
    buildServer = do
      echo ""
      echo "======================"
      echo "= Building server(s) ="
      echo "======================"
      echo ""
      cabal cfg ["build", buildDir Server]

    buildClient = do
      echo ""
      echo "======================"
      echo "=  Building  client  ="
      echo "======================"
      echo ""
      hasteCabal cfg ["build", buildDir Client]

    copyArtifact Client art = do
      cp art (artifactDir </> takeBaseName art <.> "js")
    copyArtifact Server art = do
      cp art (artifactDir </> takeFileName art)

    copyAllArtifacts client_bas server_bas = do
      mapM_ (copyArtifact Client) client_bas
      mapM_ (copyArtifact Server) server_bas
      return $ map takeFileName
             $ map (flip replaceExtension ".js") client_bas ++ server_bas

    -- embed JS into standalone binary, then copy binary to artifacts
    embedAndCopy cli srv = do
      echo ""
      echo "================================"
      echo "= Embedding client into server ="
      echo "================================"
      echo ""
      copyArtifact Server srv
      run (artifactDir </> takeFileName srv) ["-e", cli]
      return [takeFileName srv]

    reportArtifacts arts = do
      echo ""
      echo "============="
      echo "=   Done!   ="
      echo "============="
      echo ""
      echo "The following artifacts were built:"
      mapM_ (echo . ("  " ++)) arts

-- | Get all build artifacts for the given part.
findBuildArtifacts :: AppPart -> Shell [FilePath]
findBuildArtifacts part = do
    dirs <- filter (/= "autogen") <$> ls build
    exes <- zipWith getExe dirs <$> sequence [ls (build </> d) | d <- dirs]
    return $ filter (not . null) exes
  where
    build = scratchDir part </> "dist" </> "build"
    getExe dir files =
      case filter (\f -> takeBaseName f == dir) files of
        []     -> ""
        (f:fs) -> build </> dir </> f
