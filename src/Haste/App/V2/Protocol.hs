{-# LANGUAGE OverloadedStrings #-}
-- | Haste.App client-server protocol.
module Haste.App.V2.Protocol
  ( module Haste.App.V2.Protocol.Types
  , ServerException (..), ServerCall (..), ServerReply (..)
  ) where
import Control.Exception
import Control.Monad
import Data.Typeable
import GHC.StaticPtr
import Haste.Binary
import qualified Haste.Foreign as HF
import Haste.App.V2.Protocol.Types

instance HF.FromAny Endpoint where
  fromAny o = do
    hasTls <- HF.get o "tls"
    let tls = if hasTls then Just (TLSConfig "" "") else Nothing
    Endpoint <$> HF.get o "host" <*> HF.get o "port" <*> pure tls

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
  get = Endpoint <$> get <*> get <*> get
  put (Endpoint host port tls) = put host >> put port >> put tls

instance Binary TLSConfig where
  get = TLSConfig <$> get <*> get
  put (TLSConfig cert key) = put cert >> put key
