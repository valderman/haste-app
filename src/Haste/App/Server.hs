{-# LANGUAGE CPP, OverloadedStrings #-}
module Haste.App.Server (unsafeFromBlob, serverLoop) where
#ifdef __HASTE__
unsafeFromBlob _ = undefined
serverLoop _ = pure undefined
#else
import Control.Concurrent
import Control.Monad ((>=>))
import qualified Data.ByteString.Lazy as BSL
import GHC.StaticPtr
import Unsafe.Coerce
import Network.WebSockets as WS
import Haste.Binary
import Haste.App.Protocol

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WebSockets

unsafeFromBlob :: Blob -> BSL.ByteString
unsafeFromBlob = unsafeCoerce

unsafeToBlobData :: BSL.ByteString -> BlobData
unsafeToBlobData = unsafeCoerce

serverLoop :: Int -> IO ()
serverLoop port = do
    run port $ websocketsOr defaultConnectionOptions handleWS handleHttp
  where
    handleWS = acceptRequest >=> clientLoop

    clientLoop c = do
      msg <- receiveData c
      _ <- forkIO $ handlePacket c (unsafeToBlobData msg)
      clientLoop c

    handleHttp _ resp = do
      resp $ responseLBS status404 [] "WebSockets only"

handlePacket :: Connection -> BlobData -> IO ()
handlePacket c msg = do
  case decode msg of
    Right (ServerHop ep packet)          -> handleHop c ep packet
    Right (ServerCall nonce method args) -> handleCall c nonce method args
    _                                    -> error "invalid server call"

handleHop :: Connection -> Endpoint -> Blob -> IO ()
handleHop c (Endpoint host port) packet = do
  WS.runClient host port "/" $ \ c' -> do
    sendBinaryData c' $ unsafeFromBlob packet
    reply <- receiveData c'
    sendBinaryData c (reply :: BSL.ByteString)

handleCall :: Connection -> Nonce -> StaticKey -> [Blob] -> IO ()
handleCall c nonce method args = do
  mm <- unsafeLookupStaticPtr method
  case mm of
    Just m -> do
      result <- deRefStaticPtr m args
      sendBinaryData c $ unsafeFromBlob $ encode $ ServerReply
        { srNonce = nonce
        , srResult = result
        }
    _ -> do
      error $ "no such method: " ++ show method
#endif
