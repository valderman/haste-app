{-# LANGUAGE OverloadedStrings #-}
module Haste.App.Standalone.Server (runStandaloneServer) where
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.Text as T
import Haste.App
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.Directory hiding (findFile)
import System.Exit
import System.FilePath
import System.IO
import Haste.App.Standalone.Config
import Haste.App.Standalone.Embed

-- | Run application with settings obtained from the command line.
runStandaloneServer :: [EndpointConfig] -> IO ()
runStandaloneServer eps = do
  (cfg, files) <- getConfig eps
  case runMode cfg of
    Server           -> runServer [e | e@(Static _) <- eps] cfg
    Embed js         -> embedFiles cfg js files >> exitSuccess
    ListEmbedded     -> mapM_ putStrLn embeddedFiles >> exitSuccess
    PrintAndQuit msg -> putStr msg >> exitSuccess

-- | Start the HTTP server serving up the application.
runServer :: [EndpointConfig] -> Config -> IO ()
runServer staticeps cfg = do
  -- Check that we at least have a main JS file before running
  unless (jsMainExists) $ do
    hPutStrLn stderr $ "This executable does not seem to contain a " ++
                       "Haste.App client JavaScript program."
    hPutStrLn stderr $ "Please re-run it with the `--help' flag for " ++
                       "information on how to embed the\nclient JavaScript."
    exitFailure

  -- Set up working directory and print diagnostics
  setCurrentDirectory (workDir cfg)
  case endpoints cfg of
    [(_, Endpoint host _)] -> do
      let hoststr = "http://" ++ host ++ ":" ++ show (httpPort cfg)
      putStrLn $ "Application started on " ++ hoststr
    _ -> do
      putStrLn "Application started on one of:"
      forM_ (map snd $ endpoints cfg) $ \(Endpoint host _) -> do
        putStrLn $"  http://" ++ host ++ ":" ++ show (httpPort cfg)

  let allEps = staticeps ++ map (Static . snd) (endpoints cfg)
  _ <- forkIO $ runApp allEps (pure ())
  let jsMain = mkJSMain cfg
  run (httpPort cfg) $ \req respond -> do
    case T.unpack (T.intercalate "/" (pathInfo req)) of
      -- Haste.App JS file is always served embedded
      path | path == jsMainFileName -> respond $ responseLBS ok200 [] jsMain
           | otherwise              -> findFile cfg path >>= respond

-- | Find and return a file corresponding to the given path.
--   TODO: cache files to avoid having to call 'embeddedFile' all the time.
findFile :: Config -> FilePath -> IO Response
findFile cfg path
  | dataDirFirst cfg = do
    mf <- findDataFile (dataDir cfg) path
    case (mf, findEmbeddedFile path) of
      (Just f, _) -> pure $ responseFile ok200 [] f Nothing
      (_, Just f) -> pure $ responseLBS ok200 [] f
      _           -> pure $ responseLBS status404 [] "404"
  | otherwise = do
    case findEmbeddedFile path of
      Just f -> pure $ responseLBS ok200 [] f
      _      -> do
        mf <- findDataFile (dataDir cfg) path
        case mf of
          Just f -> pure $ responseFile ok200 [] f Nothing
          _      -> pure $ responseLBS status404 [] "404"

-- | Find a file in the specified data directory.
findDataFile :: Maybe FilePath -> FilePath -> IO (Maybe FilePath)
findDataFile (Just datadir) path = do
  -- Check that path exists
  mp <- catch (Just <$> (makeAbsolute (datadir </> path) >>= canonicalizePath))
              (\(SomeException _) -> pure Nothing)

  -- Check that path is inside data directory
  datadir' <- makeAbsolute datadir >>= canonicalizePath
  let index = fmap (</> "index.html") mp
  case mp of
    Just path'
      | datadir' == path'                 -> pure index
      | and $ zipWith (==) datadir' path' -> pure mp
    _                                     -> pure Nothing
findDataFile _ _ = do
  pure Nothing
