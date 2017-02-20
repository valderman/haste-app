{-# LANGUAGE OverloadedStrings, FlexibleInstances, StaticPointers #-}
import Haste.App
import Haste.Foreign

-- Change AllowAll to AllowNone to see an example of the sandbox blocking
-- things.
type MySandbox = Sandbox AllowAll ()

{- Contents of foreign.js:

   function shout(s) {
     alert(s + '!');
   }
-}

instance Node MySandbox where
  endpoint = localNode
  init = dependOn ["foreign.js"]

shout :: RemotePtr (JSString -> MySandbox ())
shout = static (remote $ liftIO . ffi "shout")

main = runApp [startLocal (Proxy :: Proxy MySandbox)] $ do
  dispatch shout "Hello, Sandbox"
