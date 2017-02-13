{-# LANGUAGE StaticPointers, FlexibleInstances #-}
import Haste.App
import Data.Typeable
import System.Random

instance Node Server where
  endpoint _ = WebSocket "1.example.com" 24601

balance :: [Endpoint] -> Client Endpoint
balance endpoints = do
  i <- liftIO $ randomRIO (0, length endpoints-1)
  return (endpoints !! i)

work :: RemotePtr (String -> Server String)
work = static (remote $ \s -> do
    liftIO $ putStrLn "Doing super heavy work!"
    return $ "Did the work: " ++ s
  )

main = runApp [start (Proxy :: Proxy Server)] $ do
  ep <- balance [WebSocket (show n ++ ".example.com") 24601 | n <- [1..5]]
  msg <- dispatchTo ep work "massive work"
  alert (toJSString (msg :: String))
