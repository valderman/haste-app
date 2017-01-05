module Commands (availableCommands) where
import Control.Shell

import Config

import Commands.Clean
import Commands.Build
import Commands.Init
import Commands.Install

-- | All available Haste.App commands.
availableCommands :: [(String, Config -> Shell ())]
availableCommands =
  [ ("init",    initialize)
  , ("build",   build)
  , ("clean",   clean)
  , ("install", install)
  ]
