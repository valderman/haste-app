{-# LANGUAGE StaticPointers, FlexibleInstances #-}
import Haste.App

instance Node Server

greet :: RemotePtr (String -> Server ())
greet = static (native $ remote $ liftIO . putStrLn)

main = runApp [start (Proxy :: Proxy Server)] $ do
  dispatch greet "Hello, server!"
