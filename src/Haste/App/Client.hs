module Haste.App.Client where
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe (isJust)
import Data.IORef
import qualified Data.Map.Strict as Map
import Haste.Binary hiding (get)
import Haste.Concurrent
import Haste.WebSockets
import Haste.App.Protocol
import Haste.App.Config (EndpointConfig, resolveEndpoint)
import qualified Haste.JSString as JSS

-- For Client MonadEvent instance
import Haste.Events (MonadEvent (..))

data Env = Env
  { nonceSupply   :: IORef Nonce
  , resultMap     :: IORef (Map.Map Nonce (MVar Blob))
  , connectionMap :: IORef (Map.Map Endpoint (WebSocket Blob))
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

instance MonadIO Client where
  liftIO = liftCIO . liftIO

instance MonadBlob Client where
  getBlobData = liftCIO . getBlobData
  getBlobText' = liftCIO . getBlobText'

instance MonadEvent Client where
  mkHandler f = do
    st <- Client return
    return $ concurrent . runClientWith st . f

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
                    <*> newIORef Map.empty)

-- | Lift a CIO action into the Client monad.
liftCIO :: CIO a -> Client a
liftCIO m = Client $ \_ -> m >>= \x -> return x

-- | Read an IORef from the session.
get :: (Env -> IORef a) -> Client a
get f = Client $ \env -> liftIO $ readIORef (f env)

-- | Create a new nonce and corresponding result MVar.
newResult :: Client (Nonce, MVar Blob)
newResult = Client $ \env -> liftIO $ do
  v <- newEmptyMVar
  n <- atomicModifyIORef' (nonceSupply env) (\n -> (n+1, n))
  atomicModifyIORef' (resultMap env) (\m -> (Map.insert n v m, ()))
  return (n, v)

-- | Connect to an endpoint. If the endpoint is already connected, the old
--   connection is closed first.
--   Returns @True@ if the connection succeeded, otherwise @False@.
reconnect :: EndpointConfig -> Client Bool
reconnect = reconnect' . resolveEndpoint

-- | Worker for 'reconnect'.
reconnect' :: Endpoint -> Client Bool
reconnect' ep = do
    r <- Client $ pure . connectionMap
    rmr <- Client $ pure . resultMap

    -- Close old connection
    mws <- liftIO $ atomicModifyIORef' r $ \cm ->
      (Map.delete ep cm, Map.lookup ep cm)
    maybe (pure ()) (liftCIO . wsClose) mws

    -- Open new one
    mws' <- liftCIO $ wsOpen (cfg rmr)
    case mws' of
      Just ws' -> liftIO $ atomicModifyIORef' r $ \cm -> (Map.insert ep ws' cm, True)
      _        -> return False
  where
    proto ep
      | isJust (endpointTLS ep) = "wss://"
      | otherwise               = "ws://"
    url = JSS.pack $ concat [proto ep, endpointHost ep, ":", show (endpointPort ep)]

    cfg rmr = noHandlers
      { wsOpenURL   = url
      , wsOnMessage = handler rmr
      }
          
    handler resmapref _ msg = do
      msg' <- getBlobData msg
      join . liftIO $ atomicModifyIORef' resmapref $ \m ->
        case decode msg' of
          Right (ServerReply nonce r)
            | Just v <- Map.lookup nonce m -> (Map.delete nonce m, putMVar v r)
            | otherwise                    -> (m, error "Bad nonce!")
          _ | Right e <- decode msg'       -> (m, throw (e :: ServerException))
            | otherwise                    -> (m, error "Bad server reply!")

-- | Send a blob to an endpoint over a web socket. If there is no open web
--   socket to the given endpoint, we open a new one. If a new connection can't
--   be opened, retry until it succeeds.
sendOverWS :: Endpoint -> Blob -> Client ()
sendOverWS ep blob = do
  -- TODO: give user control over reconnection attempts and disconnection
  --       handlers
  mws <- Map.lookup ep <$> get connectionMap
  case mws of
    Just ws -> do
      success <- liftCIO $ wsSend ws blob
      unless success $ do
        r <- Client $ pure . connectionMap
        liftIO $ do
          atomicModifyIORef' r $ \cm -> (Map.delete ep cm, ())
        liftCIO $ wsClose ws
        sendOverWS ep blob
    _       -> reconnect' ep >> sendOverWS ep blob
