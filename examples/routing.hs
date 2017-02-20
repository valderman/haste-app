{-# LANGUAGE StaticPointers, TypeFamilies, FlexibleInstances #-}
import Haste.App

data Gateway a = Gateway

instance Node Gateway where getEnv = Gateway
instance Node Server  where type Parent Server = Gateway

greet :: RemotePtr (String -> Server ())
greet = static (native $ remote $ liftIO . putStrLn)

main = runApp [start (Proxy :: Proxy Server)] $ do
  dispatch greet "Hello, routed server!"
