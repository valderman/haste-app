{-# LANGUAGE OverloadedStrings #-}
-- | Creating and reading file embeddings.
module Haste.App.Standalone.Embed
  ( embedFiles, embedFilesInto, findEmbeddedFile, embeddedFiles
  , mkJSMain, jsMainExists , jsMainFileName
  ) where
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Embed
import Data.Embed.File
import Data.Maybe (isJust)
import System.Environment (getExecutablePath)
import System.Exit
import System.Directory
import System.FilePath
import System.IO
import System.IO.Temp
import Haste.App.Standalone.Config
import Haste.App.Protocol.Types (Endpoint (..))

-- | List all files embedded in this executable.
embeddedFiles :: [FilePath]
embeddedFiles =
    [markAppFile f | f <- listBundleFiles myBundle, f /= jsFileNameFileName]
  where
    markAppFile f
      | f == jsMainFileName = '*':f
      | otherwise           = f

-- | Embed the given JS and auxiliary files into this executable.
embedFiles :: Config -> FilePath -> [FilePath] -> IO ()
embedFiles cfg js aux = do
  when (jsMainExists && not (forceEmbed cfg)) $ do
    hPutStrLn stderr $ "This executable already contains a Haste.App " ++
                       "client JavaScript program; aborting.\n" ++
                       "To embed a new client program and auxiliary " ++
                       "files, use the `--force' option."
    exitFailure
  self <- getExecutablePath
  embedFilesInto (maybe 0 id $ stripDirs cfg) self js aux

-- | Embed JS files into the given binary.
embedFilesInto :: Int -> FilePath -> FilePath -> [FilePath] -> IO ()
embedFilesInto strip bin js aux = do
    withTempFile (takeDirectory bin) "" $ \tmp h -> do
      hClose h
      BS.readFile bin >>= BS.writeFile tmp
      replaceBundle tmp $
        [ FileData jsFileNameFileName (BS.toStrict $ BS.pack jsFileName)
        , FilePath strip js
        ] ++ map (FilePath strip) aux
      copyPermissions bin tmp
      renameFile tmp bin
  where
    jsFileName = stripLeading strip js
    stripLeading 0 f = f
    stripLeading n f = stripLeading (n-1) (drop 1 (dropWhile (/= '/') f))

-- | Find an embedded file. Requests to @/@ and @/index.html@ always succeed.
findEmbeddedFile :: FilePath -> Maybe BS.ByteString
findEmbeddedFile "" =
  Just $ maybe defaultHTML BS.fromStrict (embeddedFile "index.html")
findEmbeddedFile "index.html" =
  Just $ maybe defaultHTML BS.fromStrict (embeddedFile "index.html")
findEmbeddedFile path =
  BS.fromStrict <$> embeddedFile path

-- | Embedded filename of the main JS program.
jsMainFileName :: FilePath
jsMainFileName = BS.unpack $ BS.fromStrict (embeddedFile' jsFileNameFileName)

-- | Does the JS main program exist, or do we need to embed it?
jsMainExists :: Bool
jsMainExists
  | Just _ <- embeddedFile jsMainFileName = True
  | otherwise                             = False

-- | The client application JS.
mkJSMain :: Config -> BS.ByteString
mkJSMain cfg = BS.concat
  [ BS.fromStrict (embeddedFile' jsMainFileName)
  , "window['__haste_app_endpoints']=", mkEpConf (endpoints cfg), ";"
  ]
  where
    mkEpConf = BS.intercalate "," . map mkEpEntry
    mkEpEntry (name, Endpoint h p tls) = BS.concat
      [ "{'", BS.pack name, "': "
      , "{'host': '", BS.pack h, "'"
      , ",'port': ", BS.pack (show p)
      , ",'tls':  ", if isJust tls then "true" else "false"
      , "}}"
      ]

-- | Internal name of the file that contains the actual name of the app JS
--   file.
jsFileNameFileName :: FilePath
jsFileNameFileName = "\0__appJSFileName"

-- | Default HTML skeleton.
defaultHTML :: BS.ByteString
defaultHTML = BS.concat
    [ "<!DOCTYPE HTML><html><head>"
    , "<title>Standalone Haste.App application</title>"
    , "<meta charset=\"UTF-8\">"
    , "<script type=\"text/javascript\" src=\"", js, "\"></script>"
    , "</head><body></body></html>"
    ]
  where
    js = BS.pack jsMainFileName
