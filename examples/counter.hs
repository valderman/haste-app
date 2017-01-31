{-# LANGUAGE StaticPointers, TypeFamilies, FlexibleInstances #-}
import Haste.App
import Data.IORef

type MyS = EnvServer (IORef Int)

instance Node MyS where
  type Env MyS = IORef Int
  init _ = liftIO $ newIORef 0

greet :: RemotePtr (String -> MyS Int)
greet = static (remote $ \s -> do
    ref <- ask
    liftIO $ do
      putStrLn s
      atomicModifyIORef' ref $ \n -> (n+1, n)
  )

main = runApp [start (Proxy :: Proxy MyS)] $ do
  visitor <- dispatch greet "Hello, server!"
  alert (toJSString $ "You are number " ++ show visitor)
