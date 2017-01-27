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
  ( Node (..), NodeEnv (..), MonadReader (..)
  , Tunnel
  , tunnel
  , Server, EnvServer, invokeServer, invokeEnvServer
  ) where
import Control.Monad.Reader
import Data.Default
import Data.Proxy
import Haste.Serialize -- for serialization
import Haste.JSON
import Haste.App.Protocol
import Haste.Concurrent (MonadConc (..), CIO)
import Haste (JSString, fromJSStr)
import Haste.App.Client (Client) -- for default ClientOf type instance

-- for default Endpoint instance
import Data.Typeable
import Data.Hashable
import Data.Word (Word16)
import Haste (getLocationHostName)
import System.IO.Unsafe

-- | Nest a server call in zero or more server hop packets, as directed by the
--   given path.
class Tunnel (client :: * -> *) (server :: * -> *) where
  tunnel :: Proxy client -- ^ Client to tunnel a path from
         -> Proxy server -- ^ Server to tunnel a path to
         -> ServerCall   -- ^ Server call to route
         -> (Endpoint, JSString)

-- | Base case: client and server are one and the same.
instance Tunnel client client where
  tunnel _cp _sp (ServerHop ep call) = (ep, call)
  tunnel _cp _sp _                   = error "Can't make a call to myself!"

-- | Inductive case: the current node is attached to the next node in the path.
instance {-# OVERLAPPABLE #-} (Tunnel client (ClientOf server), Node server) =>
         Tunnel client server where
  tunnel cp sp call = tunnel cp sp' $ ServerHop ep (encodeJSON $ toJSON call)
    where
      ep = endpoint sp
      sp' = Proxy :: Proxy (ClientOf server)

-- * Defining and calling servers

-- | A server node in the network.
class Node (m :: * -> *) where
  -- | The client to which this node is attached. Each node must be attached to
  --   exactly one client. This means that the attachment relation is not
  --   commutative: if @a@ is attached to @b@, then @b@ may send requests to
  --   @a@, but not the other way around.
  --   The attachments of nodes thus form a tree, rooted at the client.
  --   This is necessitated by the need for paths to be unique and unambiguous,
  --   a restriction that may or may not be possible to lift in the future.
  --   By default, nodes attach directly to 'Client'.
  type ClientOf m :: * -> *
  type ClientOf m = Client

  -- | Environment type of node. Defaults to @()@.
  type Env m :: *
  type Env m = ()

  -- | The location at which the node can be reached.
  endpoint :: Proxy m -> Endpoint
  default endpoint :: Typeable m => Proxy m -> Endpoint
  endpoint p = unsafePerformIO $ do
    let w16 = fromIntegral (hash (typeRep p)) :: Word16
        port = fromIntegral w16 `rem` (65535-1024) + 1024
#ifdef __HASTE__
    host <- fromJSStr <$> getLocationHostName
#else
    let host = ""
#endif
    return $ Endpoint host port

  -- | Initialization for the given node.
  init :: Proxy m -> CIO (Env m)
  default init :: Default (Env m) => Proxy m -> CIO (Env m)
  init _ = return def

  -- | Returns the environment of this computation.
  getEnv :: m (Env m)
  default getEnv :: MonadReader (Env m) m => m (Env m)
  getEnv = ask

  -- | Perform a computation of the given node type.
  invoke :: Env m -> m JSON -> CIO JSON
  default invoke :: (m ~ EnvServer (Env m)) => Env m -> m JSON -> CIO JSON
  invoke = invokeEnvServer

-- | Node environment tagged with its type, to avoid having to pass a Proxy
--   around to identify the type of the node.
newtype NodeEnv m = NodeEnv {unNE :: Env m}

-- | A server type, providing the base for more advanced, custom servers.
--   In order to make a simple single-server application, creating an
--   appropriate instance of 'Node' for 'Server' is all that's needed.
newtype Server a = Server {runServer :: CIO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadConc)

instance MonadReader () Server where
  ask = return ()

-- | Invoke an environment-less server computation.
invokeServer :: env -> Server a -> CIO a
invokeServer _ = runServer

-- | Invoke a server with an environment. This is the 'invoke' method when
--   creating 'Node' instances for @EnvServer@.
invokeEnvServer :: (e ~ Env (EnvServer e)) => e -> EnvServer e a -> CIO a
invokeEnvServer env = runServer . flip runReaderT env . runEnvS

-- | A server type with an environment.
newtype EnvServer e a = EnvS {runEnvS :: ReaderT e Server a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadConc, MonadReader e)

instance MonadConc (ReaderT e Server) where
  liftConc = lift . liftConc
  fork m = lift . fork . runReaderT m =<< ask
