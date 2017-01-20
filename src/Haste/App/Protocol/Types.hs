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

-- | A configurable network endpoint.
data EndpointConfig =
  -- | A static network endpoint: host, port and TLS key and certificate
  --   (if applicable) are known at compile-time.
  Static {staticEndpoint :: !Endpoint}
  -- | A dynamically configurable network endpoint with a name: the host
  --   and port of this endpoint is configured at run-time through the
  --   global @__haste_app_endpoints@ variable. This variable must be set
  --   before the Haste.App program starts executing. Example:
  --
  --     window.__haste_app_endpoints = {
  --       'my_endpoint': {'host': 'example.com', 'port': 24601, 'tls': false}
  --     };
  | Configurable {endpointName :: !String}
    deriving Show

-- | A server node startup configuration.
data NodeConfig
  = StaticNode (CIO ())
  | DynNode String (Endpoint -> CIO ())
