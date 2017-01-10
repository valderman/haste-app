{-# LANGUAGE OverloadedStrings #-}
module Haste.App.Config where
import Haste.App.Protocol
import Haste.Foreign
import System.IO.Unsafe
import Control.Exception

-- | Turn an endpoint configuration into an actual endpoint.
resolveEndpoint :: EndpointConfig -> Endpoint
resolveEndpoint (Static e) = e
resolveEndpoint (Configurable n) = unsafePerformIO $ do
  mn <- try $ configFor n
  case mn of
    Right ep               -> pure ep
    Left (SomeException _) -> error $ concat
      [ "run-time configuration missing for endpoint `" ++ n ++ "'" ]

configFor :: String -> IO Endpoint
configFor = ffi "(function(ep){return window['__haste_app_endpoints'][ep];})"
