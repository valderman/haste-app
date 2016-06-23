module Haste.App.Client where
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import GHC.StaticPtr
import System.IO.Unsafe
import qualified Data.Map.Strict as Map
import Haste.Binary hiding (get)
import Haste.Concurrent
import Haste.WebSockets
import Haste.App.Protocol

data Env = Env
  { nonceSupply   :: IORef Nonce
  , resultMap     :: IORef (Map.Map Nonce (MVar Blob))
  , connectionMap :: IORef (Map.Map Endpoint (Blob -> Client ()))
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

-- | Send a blob to an endpoint over a web socket. If there is no open web
--   socket to the given endpoint, we open a new one.
sendOverWS :: Endpoint -> Blob -> Client ()
sendOverWS ep blob = do
  -- TODO: use endpoint map to handle disconnects?
  msend <- Map.lookup ep <$> get connectionMap
  case msend of
    -- Connection already open; send message.
    Just send -> send blob

    -- No connection yet; open one.
    _         -> do
      r <- Client $ pure . connectionMap
      rmref <- Client $ pure . resultMap
      liftCIO $ do
        w <- withBinaryWebSocket url (handler rmref)
                                     (error "WebSocket error")
                                     return
        liftIO $ atomicModifyIORef' r $ \cm ->
          (Map.insert ep (liftCIO . wsSendBlob w) cm, ())
      sendOverWS ep blob
  where
    url = concat ["ws://", endpointHost ep, ":", show (endpointPort ep)]
    handler resmapref _ msg = do
      msg' <- getBlobData msg
      join . liftIO $ atomicModifyIORef' resmapref $ \m ->
        case decode msg' of
          Right (ServerReply nonce r)
            | Just v <- Map.lookup nonce m -> (Map.delete nonce m, putMVar v r)
            | otherwise                    -> (m, error "Bad nonce!")
          _ | Right e <- decode msg'       -> (m, throw (e :: ServerException))
            | otherwise                    -> (m, error "Bad server reply!")
