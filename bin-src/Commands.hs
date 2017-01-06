module Commands (availableCommands) where
import Control.Shell

import Config

import Commands.Clean
import Commands.Configure
import Commands.Build
import Commands.Delete
import Commands.Help
import Commands.Init
import Commands.Install

-- | All available Haste.App commands.
availableCommands :: [(String, Config -> Shell ())]
availableCommands =
  [ ("build",     build)
  , ("clean",     clean)
  , ("configure", void . configure)
  , ("delete",    delete)
  , ("help",      help)
  , ("init",      initialize)
  , ("install",   install)
  ]
