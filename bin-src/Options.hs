module Options where
import Control.Shell
import Data.List (foldl')
import System.Console.GetOpt

import Config

-- | All non-command command line options.
options :: [OptDescr (Config -> Config)]
options = []

run :: [(String, Config -> Shell ())] -> [String] -> Shell ()
run cmds args = do
    case (commands, errors) of
      ([c], [])
        | Just action <- lookup c cmds -> action config
        | otherwise                    -> fail $ "unrecognized command: " ++ c
      ([], _)                          -> fail "no command given"
      (_, [])                          -> fail "too many commands"
      (_, _)                           -> fail $ init $ concat errors
  where
    (cfgs, commands, errors) = getOpt Permute options args
    config = foldl' (.) id cfgs defaultConfig
