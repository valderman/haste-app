{-# LANGUAGE OverloadedStrings #-}
module Haste.App.Client
  ( Client, MonadClient (..)
  , runClient, sleep
  ) where
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe (isJust)
import Data.IORef
import qualified Data.Map.Strict as Map
import Haste.Serialize
import Haste.JSON
import Haste.Concurrent
import Haste.WebSockets
import qualified Haste.JSString as JSS
import System.IO.Unsafe
import Haste.App.Client.Type
import Haste.App.Transport
import Haste.App.Protocol
import Haste.App.Sandbox

-- For default onDisconnect handler
import Haste (toJSString, catJSStr, setTimer, Interval (..))
import Haste.DOM.JSString
import Haste.Events

-- For Client MonadEvent instance
import Haste.Events (MonadEvent (..))

-- | A barrier.
newtype Barrier = Barrier (MVar ())

-- | Open a barrier. Calling 'await' on the barrier will no longer block.
openBarrier :: Barrier -> Client ()
openBarrier (Barrier v) = liftCIO $ void $ tryPutMVar v ()

-- | Block until the given barrier becomes open. If it is already open, pass
--   immediately.
await :: Barrier -> Client ()
await (Barrier v) = liftCIO $ readMVar v

-- | Create a new barrier. The barrier is initially closed.
newBarrier :: Client Barrier
newBarrier = Barrier <$> liftCIO newEmptyMVar

-- | Sleep @n@ milliseconds.
sleep :: Int -> Client ()
sleep n = do
  b <- newBarrier
  setTimer (Once n) $ openBarrier b
  await b

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
