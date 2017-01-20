{-# LANGUAGE CPP, OverloadedStrings #-}
module Haste.App.Server (unsafeFromBlob, serverLoop) where
#ifdef __HASTE__
unsafeFromBlob _ = undefined
serverLoop _ _ = pure undefined
#else
import Control.Concurrent
import Control.Monad ((>=>))
import qualified Data.ByteString.Lazy as BSL
import GHC.StaticPtr
import Unsafe.Coerce
import Network.WebSockets as WS
import Haste.Binary
import Haste.App.Protocol
import Haste.App.Routing (NodeEnv (..))
import Haste.Concurrent (concurrent, liftIO)

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp as W
import Network.Wai.Handler.Warp.Internal as W (settingsPort)
import Network.Wai.Handler.WebSockets

unsafeFromBlob :: Blob -> BSL.ByteString
unsafeFromBlob = unsafeCoerce

unsafeToBlobData :: BSL.ByteString -> BlobData
unsafeToBlobData = unsafeCoerce

-- | Run the server event loop for a single endpoint.
serverLoop :: NodeEnv m -> Int -> IO ()
serverLoop env port = do
    run port $ websocketsOr defaultConnectionOptions handleWS handleHttp
  where
    handleWS = acceptRequest >=> clientLoop

    clientLoop c = do
      msg <- receiveData c
      _ <- forkIO $ handlePacket env c (unsafeToBlobData msg)
      clientLoop c

    handleHttp _ resp = do
      resp $ responseLBS status404 [] "WebSockets only"

handlePacket :: NodeEnv m -> Connection -> BlobData -> IO ()
handlePacket env c msg = do
  case decode msg of
    Right (ServerHop ep packet)          -> handleHop c ep packet
    Right (ServerCall nonce method args) -> handleCall env c nonce method args
    _                                    -> error "invalid server call"

handleHop :: Connection -> Endpoint -> Blob -> IO ()
handleHop c (Endpoint host port _) packet = do
  WS.runClient host port "/" $ \ c' -> do
    sendBinaryData c' $ unsafeFromBlob packet
    reply <- receiveData c'
    sendBinaryData c (reply :: BSL.ByteString)

-- | Handle a call to this specific node. Note that the method itself is
--   executed in the CIO monad by the handler.
handleCall :: NodeEnv m -> Connection -> Nonce -> StaticKey -> [Blob] -> IO ()
handleCall (NodeEnv env) c nonce method args = concurrent $ do
  mm <- liftIO $ unsafeLookupStaticPtr method
  case mm of
    Just m -> do
      result <- deRefStaticPtr m env args
      liftIO $ sendBinaryData c $ unsafeFromBlob $ encode $ ServerReply
        { srNonce = nonce
        , srResult = result
        }
    _ -> do
      error $ "no such method: " ++ show method
#endif
