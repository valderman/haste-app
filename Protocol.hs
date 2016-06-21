{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, CPP #-}
-- | Haste.App client-server protocol.
module Protocol where
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Typeable
import Data.Word
import GHC.StaticPtr
import GHC.Fingerprint.Type
import Haste.Binary

type Nonce = Int

-- | Location of a Haste.App server.
data Endpoint = Endpoint
  { endpointHost :: !String
  , endpointPort :: !Int
  } deriving (Show, Eq, Ord)

-- | A method call to the server.
data ServerCall = ServerCall
  { scNonce  :: !Nonce
  , scMethod :: !StaticKey
  , scArgs   :: ![Blob]
  } | ServerHop
  { shEndpoint :: !Endpoint
  , shPacket   :: !Blob
  }

-- | A reply to a ServerCall.
data ServerReply = ServerReply
  { srNonce  :: !Nonce
  , srResult :: !Blob
  }

-- | Throw a server exception to the client.
data ServerException = ServerException !String deriving (Typeable, Show)
instance Exception ServerException

instance Binary Word64 where
  get = do
    lo <- get :: Get Word32
    hi <- get :: Get Word32
    return $ fromIntegral lo .|. shiftL (fromIntegral hi) 32
  put x = do
    put (fromIntegral x :: Word32)
    put (fromIntegral (shiftR x 32) :: Word32)

instance Binary Fingerprint where
  get = Fingerprint <$> get <*> get
  put (Fingerprint a b) = put a >> put b

instance Binary ServerCall where
  get = do
    n <- getWord8
    case n of
      0 -> ServerCall <$> get <*> get <*> get
      1 -> ServerHop <$> get <*> get
  put (ServerCall n c as) = putWord8 0 >> put n >> put c >> put as
  put (ServerHop ep p)    = putWord8 1 >> put ep >> put p

instance Binary ServerReply where
  get = do
    n <- getWord8
    unless (n == 1) $ fail "Wrong magic byte for ServerReply"
    ServerReply <$> get <*> get
  put (ServerReply n r) = putWord8 1 >> put n >> put r

instance Binary ServerException where
  get = do
    n <- getWord8
    unless (n == 2) $ fail "Wrong magic byte for ServerException"
    ServerException <$> get
  put (ServerException e) = putWord8 2 >> put e

instance Binary Endpoint where
  get = Endpoint <$> get <*> get
  put (Endpoint host port) = put host >> put port
