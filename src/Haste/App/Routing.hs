{-# LANGUAGE UndecidableInstances,
             ScopedTypeVariables,
             TypeFamilies,
             FlexibleInstances,
             MultiParamTypeClasses,
             FlexibleContexts,
             GeneralizedNewtypeDeriving,
             CPP,
             DefaultSignatures #-}
-- | Type-level routing of requests from clients to servers attached to a
--   network and back again.
module Haste.App.Routing
  ( Node (..), NodeEnv (..), MonadReader (..), Mapping (..)
  , Server, EnvServer, invokeServer, localEndpoint
  ) where
import Control.Monad.Reader
import Data.Default
import Data.Proxy
import Haste.Serialize -- for serialization
import Haste.JSON
import Haste.App.Protocol
import Haste.Concurrent (MonadConc (..), CIO)
import Haste (JSString, fromJSStr)
import Haste.App.Client.Type (Client) -- for default Parent type instance
import Haste.App.Server.Type
import Haste.App.Transport (MonadClient)
import GHC.StaticPtr
import GHC.Exts (Constraint)

-- for default Endpoint instance
import Data.Typeable
import Data.Char
import Data.Word (Word16)
import Haste (getLocationHostName)
import System.IO.Unsafe
import System.IO (hPutStrLn, stderr)

-- * Defining and calling servers

-- | A server node in the network.
--   To define a new node @N@, the following must be provided:
--
--     * The type of the node's environment. This may be omitted if the node
--       has no environment, in which case it defaults to @()@.
--
--     * A function @endpoint@, which describes how to physically reach
--       the node; this may be omitted if the node is reachable via WebSockets
--       on the same host name as the client program is served from.
--       In this case, the port of the endpoint is determined by the hash of
--       the node's @TypeRep@.
--
--     * A function @init@, which initializes the node's environment and
--       performs any server-side initialization. This may be omitted if
--       @Env m@ is an instance of 'Default' and no other server-side setup
--       is needed.
--
--     * At least one instance of 'Mapping'. This can be omitted if @N@ is
--       a case of 'EnvServer'.
--
--   Thus, the minimal declaration to create a new node with
--   some environment @MyEnv@ would be:
--
-- > type MyNode = EnvNode MyEnv
-- > instance Node MyNode where
-- >   type Env MyNode = MyEnv
--
--   If a stateful node is desired rather than one with an environment, this
--   can be accomplishe using, for instance, @IORef@s:
--
-- > type MyNode = EnvNode (IORef MySt)
-- > instance Node MyNode where
-- >   type Env MyNode = IORef MySt
-- >   init _ = liftIO $ newIORef initialState
--
--   A stateful node which is not a case of @EnvServer@ requires slightly more
--   boilerplate:
--
-- > newtype MyNode a = MyNode (EnvServer (IORef MySt) a)
-- >   deriving (Functor, Applicative, Monad, MonadIO, MonadReader (IORef MySt))
-- > 
-- > instance Mapping MyNode a where
-- >   invoke env (MyNode m) = invokeServer env m
-- > 
-- > instance Node MyNode where
-- >   type Env MyNode = IORef MySt
-- >   init _ = liftIO $ newIORef initialState
class Node (m :: * -> *) where
  -- | Environment type of node. Defaults to @()@.
  type Env m :: *
  type Env m = ()

  -- | The type class describing all clients that may make calls to this node.
  --   By default, any 'MonadClient' may call a node.
  type Allowed m (c :: * -> *) :: Constraint
  type Allowed m c = ()

  -- | The location at which the node can be reached.
  endpoint :: Proxy m -> Endpoint
  default endpoint :: Typeable m => Proxy m -> Endpoint
  endpoint p = unsafePerformIO $ do
    let node = show $ typeRep p
        w16 = djb2 node
        port = fromIntegral w16 `rem` (65535-1024) + 1024
#ifdef __HASTE__
    host <- fromJSStr <$> getLocationHostName
#else
    hPutStrLn stderr $ "selecting port " ++ show port ++ " for node " ++ node
    let host = ""
#endif
    return $ WebSocket (if "" == host then "localhost" else host) port
    where
      -- | DJB2 hash function
      djb2 :: String -> Word16
      djb2 = go 5381
        where
          go n (c:s) = go (n*33 + (fromIntegral $ ord c)) s
          go n _     = n

  -- | Initialization for the given node.
  init :: Proxy m -> CIO (Env m)
  default init :: Default (Env m) => Proxy m -> CIO (Env m)
  init _ = return def

-- | A mapping from node return values to Haskell values.
--   This is useful when making nodes out of e.g. DSLs where the DSL-internal
--   type is not what the Haskell host program gets back from running it.
--   One instance of this is @opaleye@, another is @aplite@.
--
--   Most nodes will only need a single instance:
--
-- > instance Mapping MyNode a where
-- >   invoke env node = invokeMyNode env node
--
--   This instance is already provided for all nodes of type 'EnvServer'.
class Mapping (m :: * -> *) a where
  type Hask m a
  type Hask m a = a

  -- | Run a DSL computation which returns an @a@ on the DSL level,
  --   corresponding to @Map m a@ on the Haskell level.
  invoke :: Env m -> m a -> CIO (Hask m a)
  default invoke :: (m ~ EnvServer (Env m), Hask m a ~ a) => Env m -> m a -> CIO a
  invoke = invokeServer

-- | Node environment tagged with its type, to avoid having to pass a Proxy
--   around to identify the type of the node.
newtype NodeEnv m = NodeEnv {unNE :: Env m}

instance (t ~ Env (EnvServer t)) => Mapping (EnvServer t) a
