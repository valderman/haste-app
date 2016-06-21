{-# LANGUAGE CPP #-}
module Server (unsafeFromBlob, serverLoop) where
#ifdef __HASTE__
unsafeFromBlob _ = undefined
serverLoop _ = pure undefined
#else
import Control.Concurrent
import qualified Data.ByteString.Lazy as BSL
import GHC.StaticPtr
import Unsafe.Coerce
import Network.WebSockets as WS
import Haste.Binary
import Protocol

unsafeFromBlob :: Blob -> BSL.ByteString
unsafeFromBlob = unsafeCoerce

unsafeFromBlobData :: BlobData -> BSL.ByteString
unsafeFromBlobData = unsafeCoerce

unsafeToBlob :: BSL.ByteString -> Blob
unsafeToBlob = unsafeCoerce

unsafeToBlobData :: BSL.ByteString -> BlobData
unsafeToBlobData = unsafeCoerce

serverLoop :: Int -> IO ()
serverLoop port = do
    WS.runServer "0.0.0.0" port $ \pending -> do
      acceptRequest pending >>= clientLoop
  where
    clientLoop c = do
      msg <- receiveData c
      _ <- forkIO $ do
        case decode $ unsafeToBlobData msg of
          Right (ServerCall nonce method args) -> do
            mm <- unsafeLookupStaticPtr method
            case mm of
              Just m -> do
                result <- deRefStaticPtr m args
                sendBinaryData c $ unsafeFromBlob $ encode $ ServerReply
                  { srNonce = nonce
                  , srResult = result
                  }
              _ -> do
                error $ "Method does not exist: " ++ show method
          _ -> do
            error $ "Got bad method call: " ++ show msg
      clientLoop c
#endif
