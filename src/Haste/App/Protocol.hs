{-# LANGUAGE OverloadedStrings #-}
-- | Haste.App client-server protocol.
module Haste.App.Protocol
  ( module Haste.App.Protocol.Types
  , ServerCall (..), ServerReply (..)
  , ServerException (..), NetworkException (..)
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
  }

-- | A reply to a ServerCall.
data ServerReply = ServerReply
  { srNonce  :: !Nonce
  , srResult :: !JSON
  } | ServerEx
  { seNonce :: !Nonce
  , seMessage :: !String
  } deriving (Typeable, Show)

-- | Throw a server exception to the client.
data ServerException = ServerException String
  deriving (Typeable, Show)
instance Exception ServerException

-- | Throw an exception when there's network trouble.
data NetworkException = NetworkException String
  deriving (Typeable, Show)
instance Exception NetworkException

instance Serialize ServerCall where
  parseJSON x = do
    tag <- x .: "tag"
    case tag :: JSString of
      "call" -> ServerCall <$> x .: "nonce" <*> x .: "method" <*> x .: "args"
      _      -> fail $ "No such ServerCall constructor: " ++ show tag
  toJSON (ServerCall n c as) = Dict
    [ ("tag", "call")
    , ("nonce", toJSON n)
    , ("method", toJSON c)
    , ("args", toJSON as)
    ]

instance Serialize ServerReply where
  parseJSON x = do
    t <- x .: "status"
    case t :: JSString of
      "error" -> ServerEx <$> x .: "nonce" <*> x .: "error"
      "ok"    -> ServerReply <$> x .: "nonce" <*> x .: "result"
  toJSON (ServerReply n r) = Dict [("nonce", toJSON n), ("result", r), ("status", "ok")]
  toJSON (ServerEx n m)    = Dict [("nonce", toJSON n), ("error", toJSON m), ("status", "error")]

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
  
