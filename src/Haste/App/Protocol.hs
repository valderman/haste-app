{-# LANGUAGE OverloadedStrings #-}
-- | Haste.App client-server protocol.
module Haste.App.Protocol where
import Control.Exception
import Control.Monad
import Data.Int (Int32)
import Data.Typeable
import GHC.StaticPtr
import Haste.Binary
import qualified Haste.Foreign as HF

type Nonce = Int32

-- | Location of a Haste.App server.
data Endpoint = Endpoint
  { -- * Host on which clients can expect to reach this endpoint.
    endpointHost :: !String
    -- * Port on which the endpoint will listen to connections from clients.
  , endpointPort :: !Int
    -- * TLS certificate and key files, if applicable. Note that this
    --   information (the file names, not the files themselves) may be visible
    --   to the client for static endpoints. If this is a concern, @Node@
    --   instances should use a preprocessor @#ifdef@ to give these file names
    --   to the server while keeping them from the client:
    --
    --       instance Node YourServer where
    --         endpoint _ = Static $ Endpoint
    --           { endpointHost = someHost
    --           , endpointPort = somePort
    --       #ifdef __HASTE__
    --           , endpointTLS  = Just (TLSConfig "" "")
    --       #else
    --           , endpointTLS  = Just (TLSConfig certfile keyfile)
    --       #endif
    --           }
    --         ...
    --
    --   TLS file names specified dynamically, e.g. when using
    --   "Haste.App.Standalone", is guaranteed to never reach the client.
    --
    --   Also note that any TLS warning when using secure WebSockets will abort
    --   the connection. In particular, this means that using a self-signed
    --   certificate for testing will likely not work, unless you either tell
    --   your browser explicitly to trust that certificate, or disable
    --   certificate validation altogether during testing.
  , endpointTLS  :: !(Maybe TLSConfig)
  } deriving (Show, Eq, Ord)

-- | TLS configuration for @wss@ server.
data TLSConfig = TLSConfig
  { tlsCertFile :: FilePath
  , tlsKeyFile  :: FilePath
  } deriving (Show, Eq, Ord)

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
