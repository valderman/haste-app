{-# LANGUAGE CPP, OverloadedStrings #-}
module Haste.App.Server (serverLoop) where
import Haste.App.Transport
import Haste.App.Server.Type

#ifdef __HASTE__
serverLoop _ _ = pure undefined
instance MonadClient (EnvServer a) where
  remoteCall _ _ _ = pure undefined
#else
import Control.Concurrent
import Control.Monad ((>=>))
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.UTF8
import GHC.StaticPtr
import Unsafe.Coerce
import Network.WebSockets as WS
import Haste.Serialize
import Haste.JSON
import Haste (JSString, fromJSStr, toJSString)
import Haste.App.Protocol
import Haste.App.Routing (NodeEnv (..))
import Haste.Concurrent (concurrent, liftIO)

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp as W
import Network.Wai.Handler.Warp.Internal as W (settingsPort)
import Network.Wai.Handler.WebSockets

import Control.Exception (SomeException (..), try)

-- | Run the server event loop for a single endpoint.
serverLoop :: NodeEnv m -> Int -> IO ()
serverLoop env port = do
    run port $ websocketsOr defaultConnectionOptions handleWS handleHttp
  where
    handleWS = acceptRequest >=> clientLoop

    clientLoop c = do
      msg <- toJSString . toString <$> receiveData c
      _ <- forkIO $ handlePacket env c msg
      clientLoop c

    handleHttp _ resp = do
      resp $ responseLBS status404 [] "WebSockets only"

handlePacket :: NodeEnv m -> Connection -> JSString -> IO ()
handlePacket env c msg = do
  case decodeJSON msg >>= fromJSON of
    Right (ServerHop ep packet)          -> handleHop c ep packet
    Right (ServerCall nonce method args) -> handleCall env c nonce method args
    _                                    -> error "invalid server call"

handleHop :: Connection -> Endpoint -> JSString -> IO ()
handleHop c (WebSocket host port) packet = do
  WS.runClient host port "/" $ \ c' -> do
    sendTextData c' (fromString $ fromJSStr packet)
    reply <- receiveData c'
    sendTextData c (reply :: BSL.ByteString)
handleHop _ LocalNode{} _ = do
  error "hop to local node on server side; what to do here?"

-- | Handle a call to this specific node. Note that the method itself is
--   executed in the CIO monad by the handler.
handleCall :: NodeEnv m -> Connection -> Nonce -> StaticKey -> [JSON] -> IO ()
handleCall (NodeEnv env) c nonce method args = concurrent $ do
  mm <- liftIO $ unsafeLookupStaticPtr method
  case mm of
    Just m -> do
      result <- deRefStaticPtr m env args
      let reply = ServerReply
            { srNonce = nonce
            , srResult = result
            }
      liftIO $ sendTextData c $ fromString $ fromJSStr $ encodeJSON $ toJSON reply
    _ -> do
      error $ "no such method: " ++ show method

instance MonadClient (EnvServer a) where
  remoteCall (WebSocket h p) msg n = liftIO $ do
    WS.runClient h p "/" $ \ c' -> do
      sendTextData c' (fromString $ fromJSStr msg)
      reply <- toJSString . toString <$> receiveData c'
      case decodeJSON reply >>= fromJSON of
        Right (ServerReply n' msg) -> return msg
        Left _                     -> error "TODO: catch server exceptions"
#endif
