-- | Types for the Haste.App protocol.
module Haste.App.Protocol.Types where
import Data.Int
import Haste.Concurrent (CIO)

type Nonce = Int32

-- | Location of a Haste.App server.
data Endpoint = Endpoint
  { -- * Host on which clients can expect to reach this endpoint.
    endpointHost :: !String
    -- * Port on which the endpoint will listen to connections from clients.
  , endpointPort :: !Int
  } deriving (Show, Eq, Ord)

-- | A server node startup configuration.
type NodeConfig = CIO ()
