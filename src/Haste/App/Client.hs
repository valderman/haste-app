{-# LANGUAGE OverloadedStrings #-}
module Haste.App.Client
  ( Client, MonadClient (..)
  , runClient
  ) where
import Control.Monad ((<=<))
import Haste.Serialize
import Haste.JSON
import Haste.Concurrent
import Haste.WebSockets
import qualified Haste.JSString as JSS (concat)
import Haste.App.Client.Type
import Haste.App.Transport
import Haste.App.Protocol
import Haste (toJSString)
import Haste.App.Sandbox (callSandbox)

instance MonadClient Client where
  remoteCall ep@(LocalNode{}) pkt n = do
    liftCIO $ callSandbox n ep pkt
  remoteCall ep@(WebSocket{}) pkt n = do
      reply <- liftCIO $ do
        (v, conf) <- mkConfig ep
        Just ws <- wsOpen conf
        wsSend ws pkt
        reply <- (fromJSON <=< decodeJSON) <$> takeMVar v
        wsClose ws
        return reply
      case reply of
        Right (ServerReply _ reply) -> return reply
        Right (ServerException m)   -> throwError $ ClientError m
        Left e                      -> throwError $ ClientError (show e)
    where
      mkConfig (WebSocket host port) = do
        v <- newEmptyMVar
        return (v, WebSocketConfig
          { wsOpenURL = JSS.concat ["ws://", toJSString host, ":", toJSString port]
          , wsOnMessage = \ws x -> putMVar v x >> wsClose ws
          })
