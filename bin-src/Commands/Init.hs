-- | Initialize the Haste.App build environment.
module Commands.Init (initialize) where
import Control.Shell

import Config
import Environment

initialize :: Config -> Shell ()
initialize cfg = do
  mkdir True (scratchDir Server)
  mkdir True (scratchDir Client)
  inDirectory (scratchDir Server) $ do
    run "cabal" ["sandbox", "init"]
  inDirectory (scratchDir Client) $ do
    run "haste-cabal" ["sandbox", "init"]
