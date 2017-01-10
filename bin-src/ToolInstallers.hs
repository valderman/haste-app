-- | Installers for the standard tools.
--   Installers may assume that that @~/.haste-app/bin@ already exists.
module ToolInstallers where
import Control.Shell
import Control.Shell.Download
import Control.Shell.Extract

import Environment

hasteURI
  | os == MacOS   = "http://haste-lang.org/downloads/ghc-7.10/haste-compiler-0.5.5.1_ghc-7.10.3-darwin.tar.bz2"
  | os == Linux   = "http://haste-lang.org/downloads/ghc-7.10/haste-compiler-0.5.5.1_ghc-7.10.3-linux.tar.bz2"
  | os == Windows = error "Automatic Haste install not supported on Windows yet."

fetchAndUnpack :: URI -> Shell ()
fetchAndUnpack uri = do
  fetchFile (takeFileName uri) uri
  extractWith (defaultExtractOptions { removeArchive = True
                                     , separateDirectory = False})
              (takeFileName uri)

installHaste :: Maybe (Shell ())
installHaste =
  case os of
    Windows -> Nothing
    _       -> Just $ inAppDirectory appName $ do
      -- Check if Haste is already installed, since both Haste and haste-cabal
      -- might use this installer.
      unless (isFile $ "bin" </> "hastec") $ do
        fetchAndUnpack hasteURI
        curdir <- pwd
        run "ln" [ "-s"
                 , curdir </> "haste-compiler" </> "bin" </> "hastec"
                 , "bin" </> "hastec"
                 ]
        run "ln" [ "-s"
                 , curdir </> "haste-compiler" </> "bin" </> "haste-pkg"
                 , "bin" </> "haste-pkg"
                 ]
        run "ln" [ "-s"
                 , curdir </> "haste-compiler" </> "bin" </> "haste-cabal"
                 , "bin" </> "haste-cabal"
                 ]
    _ -> Nothing
