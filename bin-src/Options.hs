module Options where
import Control.Shell
import Data.List (foldl')
import Data.Maybe (fromJust)
import System.Console.GetOpt

import Config

-- | All non-command command line options.
options :: [OptDescr (Config -> Config)]
options =
  [ Option [] ["from-git"] (NoArg hasteAppGit) $ unlines
    [ "Use Haste.App from Git master branch instead of from Hackage."
    , "Only meaningful with `haste-app init'."
    ]
  , Option [] ["help"] (NoArg showHelp) $ unlines
    [ "Print help message and exit. Equivalent to `haste-app help'."
    ]
  ]

optionsHelp :: String
optionsHelp = unlines $ map (drop 2) $ lines $ usageInfo "" options

hasteAppGit :: Config -> Config
hasteAppGit cfg = cfg {useHasteAppGit = True}

showHelp :: Config -> Config
showHelp cfg = cfg {showHelpMessage = True}

run :: [(String, Config -> Shell ())] -> [String] -> Shell ()
run cmds args = do
    cfg <- mkConfig <$> defaultConfig (drop 1 nonopts)
    case (nonopts, errors) of
      _ | showHelpMessage cfg          -> fromJust (lookup "help" cmds) cfg
      ((c:_), [])
        | Just action <- lookup c cmds -> action cfg
        | otherwise                    -> fail $ "unrecognized command: " ++ c
      ([], _)                          -> fail "no command given; try `haste-app help'"
      (_, _)                           -> fail $ init $ concat errors
  where
    (cfgs, nonopts, errors) = getOpt Permute options args
    mkConfig = foldl' (.) id cfgs
