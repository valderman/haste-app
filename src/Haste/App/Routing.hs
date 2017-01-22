{-# LANGUAGE UndecidableInstances,
             ScopedTypeVariables,
             TypeFamilies,
             FlexibleInstances,
             MultiParamTypeClasses,
             FlexibleContexts,
             DefaultSignatures #-}
-- | Type-level routing of requests from clients to servers attached to a
--   network and back again.
module Haste.App.Routing
  ( Node (..), NodeEnv (..)
  , Tunnel
  , tunnel
  , EnvServer, invokeEnvServer, getEnvServerEnv
  ) where
import Control.Monad.Reader
import Data.Default
import Data.Proxy
import Haste.Serialize -- for serialization
import Haste.JSON
import Haste.App.Protocol
import Haste.Concurrent (MonadConc (..), CIO)
import Haste (JSString)

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
class Monad m => Node (m :: * -> *) where
  -- | The client to which this node is attached. Each node must be attached to
  --   exactly one client. This means that the attachment relation is not
  --   commutative: if @a@ is attached to @b@, then @b@ may send requests to
  --   @a@, but not the other way around.
  --   The attachments of nodes thus form a tree, rooted at the client.
  --   This is necessitated by the need for paths to be unique and unambiguous,
  --   a restriction that may or may not be possible to lift in the future.
  type ClientOf m :: * -> *

  -- | Environment type of node. Defaults to @()@.
  type Env m :: *
  type Env m = ()

  -- | Initialization for the given node.
  init :: Proxy m -> CIO (Env m)
  default init :: Default (Env m) => Proxy m -> CIO (Env m)
  init _ = return def

  -- | Returns the environment of this computation.
  getEnv :: m (Env m)
  default getEnv :: (m ~ EnvServer e) => EnvServer e e
  getEnv = EnvS $ \e -> return e

  -- | The location at which the node can be reached.
  endpoint :: Proxy m -> Endpoint

  -- | Perform a computation of the given node type.
  invoke :: Env m -> m a -> CIO a
  default invoke :: (m ~ EnvServer (Env m)) => Env m -> m a -> CIO a
  invoke = invokeEnvServer

-- | Node environment tagged with its type, to avoid having to pass a Proxy
--   around to identify the type of the node.
newtype NodeEnv m = NodeEnv {unNE :: Env m}

-- | A server type with an environment.
newtype EnvServer e a = EnvS {runEnvS :: e -> CIO a}

-- | Invoke a server with an environment. This is the 'invoke' method when
--   creating 'Node' instances for @EnvServer@.
invokeEnvServer :: (e ~ Env (EnvServer e)) => e -> EnvServer e a -> CIO a
invokeEnvServer env = flip runEnvS env

instance Functor (EnvServer e) where
  fmap f (EnvS m) = EnvS $ \e -> fmap f (m e)

instance Applicative (EnvServer e) where
  pure = return
  (<*>) = ap

instance Monad (EnvServer e) where
  return x = EnvS $ \_ -> return x
  (EnvS m) >>= f = EnvS $ \e -> m e >>= flip runEnvS e . f

instance MonadIO (EnvServer e) where
  liftIO m = EnvS $ \_ -> liftIO m

instance MonadConc (EnvServer w) where
  liftConc m = EnvS $ \_ -> liftConc m
  fork (EnvS m) = EnvS $ \e -> fork (m e)

-- | Get the environment within an @EnvServer@ computation. This is the
--   'getEnv' method when creating 'Node' instances for @EnvServer@.
getEnvServerEnv :: EnvServer e e
getEnvServerEnv = EnvS $ \e -> return e
