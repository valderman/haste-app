{-# LANGUAGE OverloadedStrings, FlexibleInstances, StaticPointers, ConstraintKinds, TypeFamilies #-}
import Haste.App
import Haste.Foreign

-- The alert call will be blocked, since the default sandbox permissions don't
-- allow modals.
data MySbx
type MySandbox = Sandbox MySbx

{- Contents of foreign.js:

   function shout(s) {
     alert(s + '!');
   }
-}

instance Node MySandbox where
  type Allowed MySandbox m = m ~ Client
  endpoint = localNode
  init = dependOn ["foreign.js"]

shout :: RemotePtr (JSString -> MySandbox ())
shout = static (remote $ liftIO . ffi "shout")

main = runApp [startLocal (Proxy :: Proxy MySandbox)] $ do
  dispatch shout "Hello, Sandbox"
