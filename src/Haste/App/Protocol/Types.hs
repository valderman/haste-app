{-# LANGUAGE RankNTypes, GADTs #-}
-- | Types for the Haste.App protocol.
module Haste.App.Protocol.Types where
import Haste.Events (Recipient (..))
import Data.Int
import Haste.Concurrent (CIO)

type Nonce = Int32

data SomeRecipient where
  SomeRecipient :: Recipient a => a -> SomeRecipient

instance Recipient SomeRecipient where
  postMessage (SomeRecipient r) = postMessage r

-- | Location of a Haste.App server.
data Endpoint
  = WebSocket
    { -- * Host on which clients can expect to reach this endpoint.
      wsEndpointHost :: !String
      -- * Port on which the endpoint will listen to connections from clients.
    , wsEndpointPort :: !Int
    }
  | LocalNode
    { -- | Name by which to identify a local endpoint.
      localNodeIdent :: !String
    } deriving (Eq, Ord, Show)

-- | A server node startup configuration.
type NodeConfig = CIO ()
