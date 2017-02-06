{-# LANGUAGE OverloadedStrings, TypeOperators, ScopedTypeVariables #-}
-- | Mid-level sandbox utilities.
module Haste.App.Sandbox.Internal
  ( createSandbox, withSandbox, isInSandbox
  , showPermissions
  , Permission, AllowForms, AllowModals, AllowOrientationLock
  , AllowPointerLock, AllowPopups, AllowPopupsEscapeSandbox, AllowPresentation
  , AllowTopNavigation, AllowAll, AllowNone
  ) where
import Control.Monad
import Data.Proxy
import Haste
import Haste.Concurrent
import Haste.DOM.JSString
import Haste.Events
import Haste.Foreign
import qualified Haste.JSString as S

-- | Sandbox restrictions that can be relaxed.
--   @allow-scripts@ is always set, and @allow-same-origin@ is disallowed since
--   it would effectively render the sandbox pointless.
data AllowNone
data AllowForms
data AllowModals
data AllowOrientationLock
data AllowPointerLock
data AllowPopups
data AllowPopupsEscapeSandbox
data AllowPresentation
data AllowTopNavigation
data a :+: b
type AllowAll
  =   AllowForms :+: AllowModals :+: AllowOrientationLock :+: AllowPointerLock
  :+: AllowPopups :+: AllowPopupsEscapeSandbox :+: AllowPresentation
  :+: AllowTopNavigation

-- | All permissions that can be given to a sandboxed program.
--   Programs can be strung together using the @:+:@ type-level operator:
-- > AllowModals :+: AllowPopups :+: AllowTopNavigation
class Permission a where
  -- | Turn a @Permission@ into the equivalent sandbox attribute.
  showPerm :: Proxy a -> JSString

-- | Turn the given permissions into a space-separated list for use with
--   the @sandbox@ DOM attribute.
showPermissions :: Permission a => Proxy a -> JSString
showPermissions p =
  case showPerm p of
    ps | S.null ps -> "allow-scripts"
       | otherwise -> "allow-scripts " `S.append` ps

instance Permission AllowNone where showPerm _ = ""
instance Permission AllowForms where showPerm _ = "allow-forms"
instance Permission AllowModals where showPerm _ = "allow-modals"
instance Permission AllowOrientationLock where showPerm _ = "allow-orientation-lock"
instance Permission AllowPointerLock where showPerm _ = "allow-pointer-lock"
instance Permission AllowPopups where showPerm _ = "allow-popups"
instance Permission AllowPopupsEscapeSandbox where showPerm _ = "allow-popups-to-escape-sandbox"
instance Permission AllowPresentation where showPerm _ = "allow-presentation"
instance Permission AllowTopNavigation where showPerm _ = "allow-top-navigation"
instance (Permission a, Permission b) => Permission (a :+: b) where
  showPerm _ = S.concat
    [ showPerm (Proxy :: Proxy a)
    , " "
    , showPerm (Proxy :: Proxy b)
    ]

-- | Low-level sandbox creation. Creates an iframe element with the sandbox
--   attribute set to the appropriate value given the list of permissions.
--   Also generates boilerplate code to tell the JS program that is's in fact
--   running in a Haste sandbox.
mkSandbox :: JSString -> IO (Elem, Window)
mkSandbox perms = do
    js <- getProgramJS
    f <- newElem "iframe" `with`
      [ "srcdoc" =: S.concat [bootstrap, prog js]
      , style "display" =: "none"
      , "sandbox" =: perms
      ]
    appendChild documentBody f
    maybe (error "impossible") (\w -> (f,w)) <$> getContentWindow f
  where
    bootstrap = S.concat
      [ "<script>"
      , "window['__haste_program_is_sandboxed'] = true;"
      , "<", "/script>"
      ]
    prog (Left url) = S.concat ["<script src=\"", url, "\"><", "/script>"]
    prog (Right js) = S.concat ["<script>", js, "<", "/script>"]

-- | Create a sandbox iframe with the given permissions,
--   then run the given callback after it finishes loading.
withSandbox :: JSString -> (Maybe Window -> IO ()) -> IO ()
withSandbox perms go = do
    local <- isInSandbox
    if local then
        go Nothing
      else do
        (e, w) <- mkSandbox perms
        void $ e `onEvent` Load $ \_ -> go (Just w)

-- | Create a sandbox with the given permissions.
--   This function works similarly to UNIX @fork@: it will return in *both*
--   the sandbox and the host program. In the sandbox it returns @Nothing@, and
--   in the host program it returns the window object that may be used to
--   communicate with the sandbox. Setting up message event listeners in the
--   responsibility of the users, in the sandbox as well as in the host program.
createSandbox :: MonadConc m => JSString -> m (Maybe Window)
createSandbox perms = liftCIO $ do
  v <- newEmptyMVar
  liftIO $ withSandbox perms (concurrent . putMVar v)
  takeMVar v

-- | Is the program running in a sandbox?
isInSandbox :: IO Bool
isInSandbox = ffi "(function(){return self['__haste_program_is_sandboxed'];})"
