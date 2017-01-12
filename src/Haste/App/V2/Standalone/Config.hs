-- | Standalone Haste.App server-side configuration and command line handling.
module Haste.App.V2.Standalone.Config where
import Data.IORef
import Data.List
import Network.Info
import System.Console.GetOpt
import System.Directory (makeAbsolute, canonicalizePath)
import System.Environment
import System.Exit
import System.IO
import System.IO.Unsafe
import qualified Haste.App.V2.Protocol.Types as Proto
import Data.Version (Version (..), showVersion)
import Paths_haste_app (version)

-- | What should we do when we start?
data RunMode
  = Server
  | PrintAndQuit String
  | Embed FilePath
  | ListEmbedded
    deriving Show

-- | Configuration for a standalone application.
data Config = Config
  { endpoints    :: [(String, Proto.Endpoint)]
  , httpPort     :: Int
  , runMode      :: RunMode
  , dataDir      :: Maybe FilePath
  , workDir      :: FilePath
  , dataDirFirst :: Bool
  , stripDirs    :: Maybe Int
  , forceEmbed   :: Bool
  } deriving Show

-- | Default configuration for a list of endpoint names.
--   Will not use TLS.
defaultConfig :: [String] -> Config
defaultConfig endpoints = Config
  { endpoints    = zip endpoints [Proto.Endpoint h p Nothing | p <- [24601..]]
  , httpPort     = 8080
  , runMode      = Server
  , dataDir      = Nothing
  , workDir      = "."
  , dataDirFirst = False
  , stripDirs    = Nothing
  , forceEmbed   = False
  }
  where h = unsafePerformIO autodetectHost

setHost :: String -> String -> Config -> Config
setHost name host c = c {endpoints = go $ endpoints c}
  where
    go (x@(name', Proto.Endpoint _ p tls):xs)
      | name == name' = (name', Proto.Endpoint host p tls):xs
      | otherwise     = x:go xs
    go []             = []

setPort :: String -> String -> Config -> Config
setPort name port c
  | [(p, "")] <- reads port = c {endpoints = go p $ endpoints c}
  | otherwise               = c
  where
    go p (x@(name', Proto.Endpoint h _ tls):xs)
      | name == name' = (name', Proto.Endpoint h p tls):xs
      | otherwise     = x:go p xs
    go _ []             = []

setTLS :: String -> String -> Config -> Config
setTLS name t c
  | Just tls <- readTLS t = c {endpoints = go (Just tls) $ endpoints c}
  | otherwise             = c
  where
    go tls (x@(name', Proto.Endpoint h p _):xs)
      | name == name' = (name', Proto.Endpoint h p tls):xs
      | otherwise     = x:go tls xs
    go _ []             = []

    readTLS s =
      case break (== ':') s of
        (cert, ':':key)
          | all (not . null) [cert, key] -> Just (Proto.TLSConfig cert key)
          | otherwise                    -> Nothing

readWithDefault :: Read a => a -> String -> a
readWithDefault def s
  | [(x, "")] <- reads s = x
  | otherwise            = def

endpointOpts :: [Char] -> [Char] -> [Char] -> String -> [OptDescr (Config -> Config)]
endpointOpts shorthost shortport shorttls name =
  [ Option shortport [name ++ "-port"]
    (ReqArg (setPort name) "PORT") $
    "Port to use for internal app communication over endpoint `" ++ name ++
    "'.\nDefault: 24601 and up"
  , Option shorthost [name ++ "-host"]
    (ReqArg (setHost name) "HOST") $
    "Host from which the endpoint `" ++ name ++ "' is served.\n" ++
    "Default: autodetect"
  , Option shorttls [name ++ "-tls"]
    (ReqArg (setTLS name) "CERTFILE:KEYFILE") $
    "Enable TLS for endpoint `" ++ name ++ "' using the given KEYFILE and " ++
    "CERTFILE.\n" ++
    "Default: don't use TLS"
  ]

optspec :: [String] -> [OptDescr (Config -> Config)]
optspec eps =
  concat (map (endpointOpts shorthost shortport shorttls) eps) ++
  [ Option "p" ["http-port"]
    (ReqArg (\p c -> c {httpPort = readWithDefault (httpPort c) p}) "PORT") $
    "Port on which users can access the application through their web " ++
    "browser.\n" ++
    "Default: 8080"

  , Option "d" ["data-directory"]
    (ReqArg (\d c -> c {dataDir = Just d}) "DIR") $
    "Directory from which to serve static files.\n" ++
    "If an embedded file exists in DIR as well, the embedded version will " ++
    "shadow the one in DIR.\n" ++
    "This means that `/' and `/index.html' will " ++
    "always be served as their embedded versions.\n" ++
    "Use `--override-embedded' to allow files in DIR to shadow embedded " ++
    "ones.\n" ++
    "Default: none"

  , Option "w" ["working-directory"]
    (ReqArg (\d c -> c {workDir = d}) "DIR") $
    "Change working directory to DIR before starting the server.\n" ++
    "Default: ."

  , Option "o" ["override-embedded"]
    (NoArg (\c -> c {dataDirFirst = True})) $
    "Allow files from the data directory to shadow embedded files.\n" ++
    "The Haste.App client JavaScript program can never be shadowed, however."
    
  , Option "e" ["embed"]
    (ReqArg (\js c -> c {runMode = Embed js}) "JS") $
    "Embed JS as the Haste.App client JavaScript file for this " ++
    "application.\n" ++
    "Any non-option arguments are interpreted as file " ++
    "names to embed alongside JS."
    
  , Option "f" ["force"]
    (NoArg (\c -> c {forceEmbed = True})) $
    "Overwrite any JavaScript and static files already embedded in this " ++
    "executable.\n"
    
  , Option "s" ["strip-dirs"]
    (ReqArg (\n c -> c {stripDirs = Just (readWithDefault 0 n)}) "N") $
    "Strip the N first leading directories of file names provided with " ++
    "`--embed'.\n" ++
    "Default: 0."

  , Option "l" ["list-files"]
    (NoArg (\c -> c {runMode = ListEmbedded})) $
    "List all files embedded in this executable. The app JavaScript file " ++
    "will be prefixed with an asterisk."

  , Option "" ["haste-app-version"]
    (NoArg (\c -> c {runMode = PrintAndQuit versionInfo})) $
    "Print the Haste.App version this program was built against."

  , Option "?" ["help"]
    (NoArg (\c -> c {runMode = PrintAndQuit (help eps)})) $
    "Print this help message."
  ]
  where
    shorthost = if length eps == 1 then "h" else ""
    shortport = if length eps == 1 then "a" else ""
    shorttls  = if length eps == 1 then "t" else ""

-- | Version info for @--haste-app-version@.
versionInfo :: String
versionInfo = "Haste.App, version " ++ showVersion version ++ "\n"

helpHeader :: String
helpHeader = concat
  [ "This web application was built using Haste.App.Standalone."
  , " It accepts the following options:"
  ]

-- | Help message for @--help@.
help :: [String] -> String
help = usageInfo helpHeader . optspec

-- | Read configuration from command line and parse it.
getConfig :: [Proto.EndpointConfig] -> IO (Config, [FilePath])
getConfig eps = do
    args <- getArgs
    case getOpt Permute (optspec endpointNames) args of
      (opts, fs, []) -> do
        let cfg = foldr (flip (.)) (id) opts (defaultConfig endpointNames)
        wd <- fixPath (workDir cfg)
        dd <- maybe (pure Nothing) (fmap Just . fixPath) (dataDir cfg)
        return (cfg {workDir = wd, dataDir = dd}, fs)
      (_, _, errs)   -> mapM_ (hPutStr stderr) errs >> exitFailure
  where
    endpointNames = [name | Proto.Configurable name <- eps]
    fixPath p = makeAbsolute p >>= canonicalizePath

-- | Attempt to autodetect the host we're currently running on.
--   Defaults to @localhost@ if no interface could be detected.
autodetectHost :: IO String
autodetectHost = do
    ifs <- map (show . ipv4) <$> getNetworkInterfaces
    return $ head $ filter isValidIpv4 ifs ++ ["localhost"]
  where
    isValidIpv4 ('0':_)         = False
    isValidIpv4 ('1':'2':'7':_) = False
    isValidIpv4 addr            = not $ and $ zipWith (==) addr "169.254"

{-# NOINLINE nextPort #-}
nextPort :: IORef Int
nextPort = unsafePerformIO $ newIORef 24601

{-# NOINLINE newPort #-}
newPort :: String -> Int
newPort _ = unsafePerformIO $ atomicModifyIORef' nextPort (\p -> (p+1, p))
