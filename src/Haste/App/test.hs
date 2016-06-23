{-# LANGUAGE StaticPointers, GeneralizedNewtypeDeriving, TypeFamilies #-}
import Haste
import App
import Data.Char
import Data.Proxy

newtype Server2 a = Server2 {runS2 :: Server a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadBlob)

instance Node Server where
  type ClientOf Server = Client
  endpoint _ = Endpoint "localhost" 24601
  invoke = invokeServer

instance Node Server2 where
  type ClientOf Server2 = Server
  endpoint _ = Endpoint "localhost" 24602
  invoke = invokeServer . runS2

rev :: String -> Client String
rev = remote $ static (import_ $ \x -> do
  annotate :: RunsOn Server
  return $ reverse x)

up :: String -> Client String
up = remote $ static (import_ $ \x -> do
  annotate :: RunsOn Server2
  return $ map toUpper x)

main = runApp [ endpoint (Proxy :: Proxy Server)
              , endpoint (Proxy :: Proxy Server2)] $ do
  prompt "INPUT TEXT LOL" >>= up >>= rev >>= liftIO . putStrLn
