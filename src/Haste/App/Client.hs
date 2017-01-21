{-# LANGUAGE OverloadedStrings #-}
module Haste.App.Client where
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
import Haste.App.Protocol
import qualified Haste.JSString as JSS

-- For default onDisconnect handler
import Haste (toJSString, catJSStr, setTimer, Interval (..))
import Haste.DOM.JSString
import Haste.Events

-- For Client MonadEvent instance
import Haste.Events (MonadEvent (..))

data Env = Env
  { nonceSupply       :: IORef Nonce
  , resultMap         :: IORef (Map.Map Nonce (MVar JSON))
  , connectionMap     :: IORef (Map.Map Endpoint (Either Barrier (WebSocket JSS.JSString)))
  , disconnectHandler :: IORef (Endpoint -> Client ())
  , reconnectHandler  :: IORef (Endpoint -> Client ())
  }

newtype Client a = Client {unC :: Env -> CIO a}

instance Functor Client where
  fmap f (Client m) = Client $ \env -> f `fmap` m env

instance Applicative Client where
  pure  = return
  (<*>) = ap

instance Monad Client where
  return x = Client $ \_ -> return x
  Client m >>= f = Client $ \env -> do
    x <- m env
    unC (f x) env

instance MonadConc Client where
  liftConc = liftCIO
  fork (Client m) = Client $ \env -> fork (m env)

instance MonadIO Client where
  liftIO = liftCIO . liftIO

instance MonadEvent Client where
  mkHandler f = do
    st <- Client return
    return $ concurrent . runClientWith st . f

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

-- | Run a client with the given environment. Absolutely do not use unless
--   you're absolutely sure what you're doing!
runClientWith :: Env -> Client () -> CIO ()
runClientWith env (Client m) = m env

-- | Run a client computation in a new environment.
--   Several clients may run concurrently, and nonces need not be globally
--   unique: each client has its own result map and its own connection to its
--   servers, and will thus not even see replies intended for other clients.
runClient :: Client () -> CIO ()
runClient (Client m) = do
    m =<< liftIO (Env <$> newIORef 0
                      <*> newIORef Map.empty
                      <*> newIORef Map.empty
                      <*> newIORef dropped
                      <*> newIORef reconnected)
  where
    -- TODO: handle multiple simultaneous disconnects more nicely
    reconnectId = "__haste_app_reconnect_cover"
    dropped ep = do
      cover <- newElem "div" `with`
        [ style "position" =: "fixed"
        , style "top" =: "0"
        , style "left" =: "0"
        , style "width" =: "100%"
        , style "height" =: "100%"
        , "id" =: reconnectId
        , style "background-color" =: "rgba(255, 255, 255, 0.6)"
        , style "text-align" =: "center"
        , style "font-size" =: "24pt"
        , "innerText" =: "Connection lost; reconnecting..."
        , style "padding-top" =: "10em"
        ]
      appendChild documentBody cover
      keepRetrying 250 ep cover

    updateMsg e 0 = do
      setProp e "innerText" "Connection lost; reconnecting..."
    updateMsg e n = do
      setProp e "innerText" $ catJSStr ""
        ["Connection lost; reconnecting in ", toJSString n, " seconds..."]
      void $ setTimer (Once 1000) $ updateMsg e (n-1)

    -- 10 minutes max reconnection delay
    maxDelay = 10*60*1000

    keepRetrying n ep e = do
      updateMsg e (n `div` 1000)
      success <- reconnect ep
      if success
        then do
          withElem reconnectId $ \e -> do
            deleteChild documentBody e
        else do
          sleep n
          keepRetrying (min maxDelay (n*2)) ep e

    reconnected ep = return ()

-- | Lift a CIO action into the Client monad.
liftCIO :: CIO a -> Client a
liftCIO m = Client $ \_ -> m >>= \x -> return x

-- | Read an IORef from the session.
get :: (Env -> IORef a) -> Client a
get f = Client $ \env -> liftIO $ readIORef (f env)

-- | Read an IORef from the session.
getR :: (Env -> IORef a) -> Client (IORef a)
getR f = Client $ pure . f

-- | Atomically and strictly update an IORef in the session.
update :: (Env -> IORef a) -> (a -> (a, b)) -> Client b
update ref f = Client $ \env -> do
  liftIO $ atomicModifyIORef' (ref env) f

-- | Create a new nonce and corresponding result MVar.
newResult :: Client (Nonce, MVar JSON)
newResult = Client $ \env -> liftIO $ do
  v <- newEmptyMVar
  n <- atomicModifyIORef' (nonceSupply env) (\n -> (n+1, n))
  atomicModifyIORef' (resultMap env) (\m -> (Map.insert n v m, ()))
  return (n, v)

-- | Perform the given computation whenever a connection is lost.
--   Note that this handler is responsible for restoring the dropped
--   connection. The default handler repeatedly tries to reconnect with
--   exponential backoff; it is highly recommended that any custom
--   disconnection handlers to the same.
onDisconnect :: (Endpoint -> Client ()) -> Client ()
onDisconnect handler = update disconnectHandler $ \_ -> (handler, ())

-- | Perform the given computation whenever a previously lost computation
--   is restored.
onReconnect :: (Endpoint -> Client ()) -> Client ()
onReconnect handler = update reconnectHandler $ \_ -> (handler, ())

-- | Connect to an endpoint. If the endpoint is already connected, the old
--   connection is kept open instead. If a dropped connection is restored,
--   the @onReconnect@ handler will be called.
--   Returns @True@ if the connection succeeded, otherwise @False@.
reconnect :: Endpoint -> Client Bool
reconnect ep = do
    -- Open new connection
    rmr <- getR resultMap
    mws <- liftCIO $ wsOpen (cfg rmr)
    case mws of
      Just ws -> do
        mbarrier <- update connectionMap $ \cm ->
          case Map.lookup ep cm of
            -- If we're reconnecting a closed socked, call reconnect handler
            Just (Left barrier) -> (Map.insert ep (Right ws) cm, Just barrier)
            -- If we're reconnecting an open socket, just leave the old one open
            Just (Right _)      -> (cm, Nothing)
            -- If we're connecting for the first time, don't call any handlers
            _                   -> (Map.insert ep (Right ws) cm, Nothing)
        flip (maybe (return ())) mbarrier $ \barrier -> do
          reconnected <- get reconnectHandler
          reconnected ep
          openBarrier barrier
        return True
      _ -> do
        return False
  where
    url = JSS.pack $ concat ["wss://", endpointHost ep, ":", show (endpointPort ep)]

    cfg rmr = noHandlers
      { wsOpenURL   = url
      , wsOnMessage = handler rmr
      }
          
    handler resmapref _ msg = do
      join . liftIO $ atomicModifyIORef' resmapref $ \m ->
        case decodeJSON msg >>= fromJSON of
          Right (ServerReply nonce r)
            | Just v <- Map.lookup nonce m ->
                (Map.delete nonce m, putMVar v r)
            | otherwise ->
                (m, error "Bad nonce!")
          _ | Right e <- decodeJSON msg >>= fromJSON ->
                (m, throw (e :: ServerException))
            | otherwise ->
                (m, error "Bad server reply!")

-- | Send a blob to an endpoint over a web socket. If there is no open web
--   socket to the given endpoint, we open a new one. If a new connection can't
--   be opened, retry until it succeeds.
sendOverWS :: Endpoint -> JSS.JSString -> Client ()
sendOverWS ep json = do
  mws <- Map.lookup ep <$> get connectionMap
  case mws of
    -- If we found a WebSocket, we attempt to make a call.
    Just (Right ws) -> do
      success <- liftCIO $ wsSend ws json
      unless success $ handleConnectionFailure ep json (Just ws)

    -- If we found a barrier, wait for it to open and then retry.
    Just (Left barrier) -> do
      await barrier
      sendOverWS ep json

    -- If we found nothing, the endpoint hasn't previously been connected, so
    -- try to connect first.
    _ -> do
      success <- reconnect ep
      if success
        then sendOverWS ep json
        else handleConnectionFailure ep json Nothing

-- | Call the disconnection handler on the given endpoint, wait until
--   reconnect, and then retry the failed send.
--
--   The last argument gives the previous socket for the endpoint, if any.
--   This is used to determine whether someone else has already handled the
--   failure by opening a new one and inserting it into the endpoint map.
handleConnectionFailure :: Endpoint -> JSS.JSString -> Maybe (WebSocket JSS.JSString) -> Client ()
handleConnectionFailure ep json mws = do
  -- If the call fails, we must check if we're the first to detect failure.
  -- If we're the first to detect failure (that is, the socket is still
  -- there, and it's the same socket so it hasn't been reconnected while we
  -- weren't looking), replace it with a barrier and call the disconnect
  -- handler. The barrier will be opened when the socket is reconnected.
  -- If there was no previous socket for the given endpoint, we're obviously
  -- the first to detect the failure.
  bar <- newBarrier
  callHandler <- update connectionMap $ \cm ->
    case Map.lookup ep cm of
      Just (Right ws)
        | maybe True (== ws) mws -> (Map.insert ep (Left bar) cm, True)
      Nothing                    -> (Map.insert ep (Left bar) cm, True)
      _                          -> (cm, False)
  when callHandler $ do
    disconnect <- get disconnectHandler
    disconnect ep
  await bar
  sendOverWS ep json
