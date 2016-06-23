-- | Haste.App client-server protocol.
module Haste.App.Protocol where
import Control.Exception
import Control.Monad
import Data.Typeable
import GHC.StaticPtr
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

instance Binary ServerCall where
  get = do
    n <- getWord8
    case n of
      0 -> ServerCall <$> get <*> get <*> get
      1 -> ServerHop <$> get <*> get
      _ -> fail $ "No such ServerCall constructor: " ++ show n
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
