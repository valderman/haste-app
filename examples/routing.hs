{-# LANGUAGE StaticPointers, TypeFamilies, FlexibleInstances #-}
import Haste.App

newtype Gateway a = Gateway (Server a)

instance Node Gateway where getEnv = Gateway $ pure ()
instance Node Server  where type ClientOf Server = Gateway

greet :: RemotePtr (String -> Server ())
greet = static (native $ remote $ liftIO . putStrLn)

main = runApp [start (Proxy :: Proxy Server)] $ do
  dispatch greet "Hello, routed server!"
