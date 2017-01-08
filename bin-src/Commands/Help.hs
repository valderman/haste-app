-- | Display help message.
module Commands.Help (help) where
import Control.Shell
import Data.Version
import Config
import Environment
import Paths_haste_app

help :: Config -> Shell ()
help cfg =
  case extraArgs cfg of
    []                       -> mainHelp
    [s] | s == appConfigFile -> appConfigHelp
    _                        -> fail "unknown help target; try `haste-app help'"

mainHelp :: Shell ()
mainHelp = echo $ init $ unlines
  [ "========================================="
  , "= Haste.App build tool, version " ++ showVersion version ++ " ="
  , "========================================="
  , ""
  , "Builds Haste.App applications, including standalone apps, in one go."
  , ""
  , "Available commands:"
  , "  build     - build all project targets. Targets and their settings are read"
  , "              from the application configuration file `" ++ appConfigFile ++ "',"
  , "              if present. Otherwise, if the project only has a single"
  , "              executable, and that executable uses Haste.App.Standalone or"
  , "              Haste.App.Simple, the build tool will embed the client into"
  , "              the server automatically."
  , "              For more information about the application configuration file,"
  , "              see `haste-cabal help " ++ appConfigFile ++ "'."
  , "  clean     - remove temporary build files."
  , "  configure - run Cabal configuration for app."
  , "  delete    - delete the Haste.App build environment."
  , "  help      - print this message."
  , "  init      - create a new Haste.App build environment."
  , "  install   - install one or more packages into the build environment, for both"
  , "              the client and the server parts."
  , "  setup     - install all client and server dependencies, as well as any"
  , "              programs that can be automatically installed."
  ]

appConfigHelp :: Shell ()
appConfigHelp = echo $ init $ unlines
  [ "================================================"
  , "= The Haste.App application configuration file ="
  , "================================================"
  , ""
  , "The per-application configuration file, " ++ appConfigFile ++ ", is a JSON file"
  , "containing information about each build target of the application."
  , "It does not replace the application's `.cabal' file, but augments it with"
  , "additional information."
  , ""
  , "On the top level, the file consists of a JSON object with the following keys:"
  , "  targets     - a list of target specifications. A target specification may be"
  , "                either an object containing a `name' key and zero or more of"
  , "                the `type', `embed-dir' and `embed-files' keys, or a string"
  , "                specifying its name."
  , "                If a target is specified only as a string, it inherits the"
  , "                default `type', etc. values from the top level configuration."
  , "                If a target is an object, each of the `type', etc. keys that"
  , "                is present will override the default value of that key."
  , ""
  , "  type        - the default type of build targets. May be either `client',"
  , "                `server', `both' or `standalone'. Each target with the `client'"
  , "                type gets built only with the Haste compiler, each target with"
  , "                the `server'  type only gets built with vanilla GHC, and each "
  , "                target with the `both' type gets built with both."
  , "                Default: \"both\"."
  , ""
  , "                For targets with the `standalone' type, the build tool first"
  , "                builds the target using both compilers. Then, the client "
  , "                program and any files specified using `embed-files' are"
  , "                embedded into the server program, to create a standalone"
  , "                binary."
  , "                Use this target type only when your application uses"
  , "                Haste.App.Standalone or Haste.App.Simple."
  , ""
  , "  embed-dir   - path to directory in which extra files to be embedded into"
  , "                standalone executables reside. Can be overriden per target."
  , "                Must be relative to project root directory."
  , "                Default: `\".\"'"
  , ""
  , "  embed-files - extra files to embed into standalone executable for all targets."
  , "                Can be overriden per target."
  , "                Default: `[]'"
  , ""
  , "Alternatively, the `targets' field may be replaced with a `name' field."
  , "This indicates that the application only has a single target, with that name."
  , "The top level object must have exactly one of the keys `targets` and `name'."
  ]
