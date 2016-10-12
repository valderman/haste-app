{-# LANGUAGE OverloadedStrings #-}
module Haste.App.Config where
import qualified Haste.App.Protocol as Proto
import Haste.Foreign
import System.IO.Unsafe
import Control.Exception

-- | A configurable network endpoint.
data EndpointConfig =
  -- | A static network endpoint: host, port and TLS key and certificate
  --   (if applicable) are known at compile-time.
  Static {staticEndpoint :: !Proto.Endpoint}
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

-- | Turn an endpoint configuration into an actual endpoint.
resolveEndpoint :: EndpointConfig -> Proto.Endpoint
resolveEndpoint (Static e) = e
resolveEndpoint (Configurable n) = unsafePerformIO $ do
  mn <- try $ configFor n
  case mn of
    Right ep               -> pure ep
    Left (SomeException _) -> error $ concat
      [ "run-time configuration missing for endpoint `" ++ n ++ "'" ]

configFor :: String -> IO Proto.Endpoint
configFor = ffi "(function(ep){return window['__haste_app_endpoints'][ep];})"
