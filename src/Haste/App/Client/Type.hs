{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The @Client@ monad type.
module Haste.App.Client.Type where
import Haste.Concurrent (MonadConc, CIO)
import Control.Monad.IO.Class (MonadIO)
import Haste.Events (MonadEvent)

-- | A client program running in the browser.
newtype Client a = Client {runClient :: CIO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadConc, MonadEvent)
