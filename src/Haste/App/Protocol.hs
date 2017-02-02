{-# LANGUAGE OverloadedStrings #-}
-- | Haste.App client-server protocol.
module Haste.App.Protocol
  ( module Haste.App.Protocol.Types
  , ServerException (..), ServerCall (..), ServerReply (..)
  ) where
import Control.Exception
import Control.Monad
import Data.Typeable
import GHC.StaticPtr
import Haste.Serialize
import Haste.JSON
import qualified Haste.Foreign as HF
import Haste.App.Protocol.Types
import GHC.Fingerprint.Type
import Data.Bits
import Data.Word
import Haste (JSString)

-- | A method call to the server.
data ServerCall = ServerCall
  { scNonce  :: !Nonce
  , scMethod :: !StaticKey
  , scArgs   :: ![JSON]
  } | ServerHop
  { shEndpoint :: !Endpoint
  , shPacket   :: !JSString
  }

-- | A reply to a ServerCall.
data ServerReply = ServerReply
  { srNonce  :: !Nonce
  , srResult :: !JSON
  }

-- | Throw a server exception to the client.
data ServerException = ServerException !String deriving (Typeable, Show)
instance Exception ServerException

instance Serialize ServerCall where
  parseJSON x = do
    tag <- x .: "tag"
    case tag :: JSString of
      "call" -> ServerCall <$> x .: "nonce" <*> x .: "method" <*> x .: "args"
      "hop"  -> ServerHop <$> x .: "endpoint" <*> x .: "packet"
      _      -> fail $ "No such ServerCall constructor: " ++ show tag
  toJSON (ServerCall n c as) = Dict
    [ ("tag", "call")
    , ("nonce", toJSON n)
    , ("method", toJSON c)
    , ("args", toJSON as)
    ]
  toJSON (ServerHop ep p) = Dict
    [ ("tag", "hop")
    , ("endpoint", toJSON ep)
    , ("packet", toJSON p)
    ]

instance Serialize ServerReply where
  parseJSON x = ServerReply <$> x .: "nonce" <*> x .: "result"
  toJSON (ServerReply n r) = Dict [("nonce", toJSON n), ("result", r)]

instance Serialize ServerException where
  parseJSON x = ServerException <$> parseJSON x
  toJSON (ServerException e) = toJSON e

instance Serialize Endpoint where
  parseJSON x = do
    t <- x .: "type"
    case t :: JSString of
      "websocket" -> WebSocket <$> x .: "host" <*> x .: "port"
      "local"     -> LocalNode <$> x .: "name"
      _           -> fail "unknown endpoint type"
  toJSON (WebSocket h p)  = Dict [("type", "websocket"), ("host", toJSON h), ("port", toJSON p)]
  toJSON (LocalNode name) = Dict [("type", "local"), ("name", toJSON name)]

instance Serialize Word64 where
  parseJSON x = do
    lo <- x .: "lo" :: Parser Double
    hi <- x .: "hi" :: Parser Double
    return $ round lo .|. shiftL (round hi) 32
  toJSON x = Dict
    [ ("lo", toJSON (fromIntegral (x .&. 0xffffffff) :: Double))
    , ("hi", toJSON (fromIntegral (shiftR x 32 .&. 0xffffffff) :: Double))
    ]

instance Serialize Fingerprint where
  parseJSON x = Fingerprint <$> x .: "lo" <*> x .: "hi"
  toJSON (Fingerprint lo hi) = Dict
    [ ("lo", toJSON lo)
    , ("hi", toJSON hi)
    ]
  
