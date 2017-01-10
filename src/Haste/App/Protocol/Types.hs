-- | Types for the Haste.App protocol.
module Haste.App.Protocol.Types where
import Data.Int

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
