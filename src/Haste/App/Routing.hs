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
  ) where
import Control.Monad.Reader
import Data.Default
import Data.Proxy
import Haste.Serialize -- for serialization
import Haste.JSON
import Haste.App.Protocol
import Haste.Concurrent (CIO)
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
  default getEnv :: m ()
  getEnv = return ()

  -- | The location at which the node can be reached.
  endpoint :: Proxy m -> Endpoint

  -- | Perform a computation of the given node type.
  invoke :: Env m -> m a -> CIO a

-- | Node environment tagged with its type, to avoid having to pass a Proxy
--   around to identify the type of the node.
newtype NodeEnv m = NodeEnv {unNE :: Env m}
