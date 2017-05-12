{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
import Haste.App

newtype LoggingServer a = L (Server a)
  deriving (Functor, Applicative, Monad, MonadIO)

warn :: RemotePtr (String -> LoggingServer ())
warn = static (remote $ liftIO . putStrLn . ("WARNING: " ++))

serverComputation :: RemotePtr (Server ())
serverComputation = static (remote $ do
    dispatch warn "warning from server"
  )

instance Node LoggingServer
instance Node Server

instance Mapping LoggingServer a where
  invoke e (L m) = invokeServer e m

main = runApp
  [ start (Proxy :: Proxy LoggingServer)
  , start (Proxy :: Proxy Server)
  ] $ do
  alert "Logging from the client!"
  dispatch warn "warning from client"
  dispatch serverComputation
