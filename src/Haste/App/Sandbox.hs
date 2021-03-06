{-# LANGUAGE GADTs, OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
-- | Sandboxed FFI for Haste.App.
module Haste.App.Sandbox
  ( invokeSandbox, dependOn, withDepends
  , Perms (..), CustomSandbox, Sandbox, module Sbx
  , callSandbox, createAppSandbox, initAppSandbox, isInSandbox
  ) where
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Default
import Data.Proxy
import Data.Typeable
import GHC.StaticPtr
import Haste
import Haste.Concurrent
import Haste.Events
import Haste.Foreign
import Haste.JSON
import Haste.Serialize
import Haste.App.Protocol
import Haste.App.Routing
import Haste.App.Sandbox.Internal
import qualified Haste.App.Sandbox.Internal as Sbx
  hiding (createSandbox, withSandbox, isInSandbox)

-- For dynamically loading JS dependencies.
import Haste.DOM.JSString

registerSbx :: String -> Window -> IO ()
registerSbx = ffi "(function(name,sbx){window['__haste_app_sbx'][name] = sbx;})"

lookupSbx :: String -> IO (Maybe Window)
lookupSbx = ffi "(function(name){return window['__haste_app_sbx'][name];})"

initSbxRegistry :: IO ()
initSbxRegistry = ffi "(function(){if(!window['__haste_app_sbx']) {window['__haste_app_sbx'] = {};};})"

-- | Returns @True@ if run from a sandbox which has already been initialized,
--   otherwise @False@. Also marks the sandbox as initialized.
initialized :: IO Bool
initialized = ffi "(function(){\
    \var prev = !!window['__haste_app_sbx_inited'];\
    \window['__haste_app_sbx_inited'] = true;\
    \return prev;\
  \})"

-- | Called in sandbox to report to host that it's done loading dependencies.
doneLoadingDeps :: IO ()
doneLoadingDeps = ffi "(function(){parent.postMessage(__haste_prog_id,'*');})"

-- | Create a Haste.App sandbox and set it up to listen to requests.
--   Only used internally, to start 'LocalNode' nodes. Called OUTSIDE sandbox
--   only.
createAppSandbox :: forall c m env.
                    (Perms m, Node m)
                 => Proxy m
                 -> CIO ()
createAppSandbox p = do
    Just sbx <- createSandbox (perms p)
    awaitLoadingDeps
    liftIO $ do
      initSbxRegistry
      registerSbx ident sbx
  where
    awaitLoadingDeps = do
      v <- newEmptyMVar
      pid <- getProgramId
      h <- window `onEvent` Message $ \msg -> do
        md <- liftIO $ fromAny (messageData msg)
        when (md == pid) $ putMVar v ()
      takeMVar v
      unregisterHandler h
    LocalNode ident = endpoint p

-- | Initialize a sandbox. Called INSIDE the sandbox only.
initAppSandbox :: (Perms m, Node m) => Proxy m -> CIO ()
initAppSandbox p = do
    alreadyInited <- liftIO initialized
    unless alreadyInited $ do
      _ <- initSandbox =<< Haste.App.Routing.init p
      liftIO $ doneLoadingDeps
      return ()
  where
    initSandbox env = do
      window `onEvent` Message $ \msg -> do
        mess <- liftIO $ fromAny (messageData msg)
        case fromJSON =<< decodeJSON mess of
          Right (ServerCall n m a) -> handleCall (messageSource msg) env n m a
          Left _                   -> return ()

    handleCall parent env nonce method args = do
      mm <- liftIO $ unsafeLookupStaticPtr method
      case mm of
        Just m -> do
          result <- deRefStaticPtr m env args
          postMessage parent $ encodeJSON $ toJSON $ ServerReply
            { srNonce = nonce
            , srResult = result
            }
        Nothing -> do
          postMessage parent $ encodeJSON $ toJSON $
            ServerEx nonce $ "no such method: " ++ show method


-- | Send a 'ServerCall' to a sandbox and wait for a reply.
callSandbox :: Nonce -> Endpoint -> JSString -> CIO JSON
callSandbox nonce (LocalNode ident) outgoing = do
  msbx <- liftIO $ lookupSbx ident
  case msbx of
    Nothing  -> error $ "no such sandbox: " ++ ident
    Just sbx -> do
      v <- newEmptyMVar
      h <- window `onEvent` Message $ \msg -> do
        m <- liftIO $ fromAny (messageData msg)
        case fromJSON =<< decodeJSON m of
          Right (ServerReply n result) | nonce == n -> do
            preventDefault
            putMVar v result
          _ -> do
            return ()
      postMessage sbx outgoing
      res <- takeMVar v
      unregisterHandler h
      return res

-- | A standard sandbox, with no environment and no extra permissions.
type Sandbox = CustomSandbox AllowNone ()

class Perms m where
  perms :: Proxy (m :: * -> *) -> JSString

instance Permission perms => Perms (CustomSandbox perms env t) where
  perms _ = showPermissions (Proxy :: Proxy perms)

-- | A node executing in a sandboxed iframe. The permissions of the sandbox
--   are given by the @perm@ type argument; see 'Permission' for details.
newtype CustomSandbox perm env t a = Sandbox (EnvServer env a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadConc, MonadReader env)

-- | Invoke a sandboxed computation. Use for @Mapping Sandbox@ instances.
--   However, in pretty much all circumstances, the provided @Mapping@ instance
--   should be the only one needed for any sandbox.
invokeSandbox :: env -> CustomSandbox perm env t a -> CIO a
invokeSandbox env (Sandbox m) = invokeServer env m

-- | Initialize a sandbox by loading the given dependencies and then running
--   the given initialization function. Dependencies are loaded in order,
--   in case some of them depend on each other.
--   Use this for the @init@ method when creating @Node@ instances for
--   sandboxes:
--
-- > instance Node (Sandbox SomePermissions SomeEnv) where
-- >   init = initializeStuff `withDepends` ["http://.../script.js", ...]
-- >   ...
withDepends :: CIO env -> [URL] -> Proxy (m :: * -> *) -> CIO env
withDepends extraInit deps _ = do
    v <- newEmptyMVar
    addScript v deps
    takeMVar v
  where
    addScript v (url:us) = do
      e <- newElem "script"
      appendChild documentBody e
      e `onEvent` Load $ \_ -> addScript v us
      setProp e "src" url
    addScript v _ = do
      extraInit >>= putMVar v

-- | Initialize a sandbox by loading the given dependencies and then returning
--   the default value for the sandbox' environment.
dependOn :: Default env => [URL] -> Proxy (m :: * -> *) -> CIO env
dependOn deps = pure def `withDepends` deps

instance env ~ Env (CustomSandbox perm env t) => Mapping (CustomSandbox perm env t) a where
  invoke = invokeSandbox
