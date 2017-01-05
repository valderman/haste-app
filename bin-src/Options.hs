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
    defConfig <- defaultConfig (drop 1 nonopts)
    case (nonopts, errors) of
      ((c:_), [])
        | Just action <- lookup c cmds -> action (mkConfig defConfig)
        | otherwise                    -> fail $ "unrecognized command: " ++ c
      ([], _)                          -> fail "no command given; try `haste-app help'"
      (_, _)                           -> fail $ init $ concat errors
  where
    (cfgs, nonopts, errors) = getOpt Permute options args
    mkConfig = foldl' (.) id cfgs
