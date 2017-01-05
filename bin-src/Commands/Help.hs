-- | Display help message.
module Commands.Help (help) where
import Control.Shell
import Data.Version
import Config
import Paths_haste_app

help :: Config -> Shell ()
help _ = echo $ init $ unlines
  [ "========================================="
  , "= Haste.App build tool, version " ++ showVersion version ++ " ="
  , "========================================="
  , ""
  , "Builds Haste.App applications, including standalone apps, in one go."
  , ""
  , "Available commands:"
  , "  build     - build all executables in project's cabal file."
  , "              If the project only has a single executable, and that executable"
  , "              uses Haste.App.Standalone or Haste.App.Simple, the build tool"
  , "              will embed the client into the server automatically."
  , "  clean     - remove temporary build files."
  , "  configure - run Cabal configuration for app."
  , "  delete    - delete the Haste.App build environment."
  , "  help      - print this message."
  , "  init      - create a new Haste.App build environment."
  , "  install   - install one or more packages into the build environment, for both"
  , "              the client and the server parts."
  ]
