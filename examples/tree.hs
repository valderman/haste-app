{-# LANGUAGE StaticPointers, TypeFamilies, FlexibleInstances #-}
import Haste.App
import Data.IORef

type MyS = EnvServer (IORef Int)

instance Node Server

-- Note that MyS attaches to Server, instead of the default Client.
-- Server then in turn attaches to Client, ensuring that MyS is reachable,
-- but with a detour via Server. In addition, this enables Server to make
-- requests to MyS.
instance Node MyS where
  type Parent MyS = Server
  type Env MyS = IORef Int
  init _ = liftIO $ newIORef 0

greet :: RemotePtr (String -> MyS Int)
greet = static (native $ remote $ \s -> do
    ref <- ask
    liftIO $ do
      putStrLn s
      atomicModifyIORef' ref $ \n -> (n+1, n)
  )

indirectGreeting :: RemotePtr (String -> Server String)
indirectGreeting = static (native $ remote $ \s -> do
    liftIO $ putStrLn "On Server now..."
    res <- dispatch greet $ "Greeting via Server: " ++ s
    return $ "You're number " ++ show res ++ "!"
  )

main = runApp [start (Proxy :: Proxy Server), start (Proxy :: Proxy MyS)] $ do
  dispatch greet "Hello directly, mr. Server!"
  msg <- dispatch indirectGreeting "Hello!"
  alert (toJSString msg)
