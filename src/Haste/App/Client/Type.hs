{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The @Client@ monad type.
module Haste.App.Client.Type
  ( MonadError (..), MonadConc (..), MonadEvent (..), MonadIO (..)
  , Client, ClientError (..)
  , runClient
  ) where
import Haste.Concurrent (MonadConc (..), CIO, concurrent)
import Control.Monad.IO.Class (MonadIO)
import Haste.Events (MonadEvent (..))

import Control.Exception
import Control.Monad.Error
import Data.Typeable

data ClientError = ClientError String
  deriving (Typeable, Show)

instance Error ClientError where
  strMsg = ClientError

instance (Error e, MonadConc m) => MonadConc (ErrorT e m) where
  liftCIO = lift . liftCIO
  fork m  = lift $ fork $ do
    Right x <- runErrorT m
    return x

instance MonadEvent Client where
  mkHandler f = return $ \x -> concurrent $ do
    void $ runClient $ f x

runClient :: Client a -> CIO a
runClient (Client m) = do
  res <- runErrorT m
  case res of
    Right x              -> pure x
    Left (ClientError e) -> error e

-- | A client program running in the browser.
newtype Client a = Client {unClient :: ErrorT ClientError CIO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadConc, MonadError ClientError)
