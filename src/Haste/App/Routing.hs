{-# LANGUAGE UndecidableInstances,
             ScopedTypeVariables,
             TypeFamilies,
             FlexibleInstances,
             MultiParamTypeClasses #-}
-- | Type-level routing of requests from clients to servers attached to a
--   network and back again.
module Haste.App.Routing
  ( Node (..)
  , Tunnel
  , tunnel
  ) where
import Data.Proxy
import Haste.Binary -- for serialization
import Haste.App.Protocol
import Haste.App.Config

-- | Nest a server call in zero or more server hop packets, as directed by the
--   given path.
class Tunnel (client :: * -> *) (server :: * -> *) where
  tunnel :: Proxy client -- ^ Client to tunnel a path from
         -> Proxy server -- ^ Server to tunnel a path to
         -> ServerCall   -- ^ Server call to route
         -> (Endpoint, Blob)

-- | Base case: client and server are one and the same.
instance Tunnel client client where
  tunnel _cp _sp (ServerHop ep call) = (ep, call)
  tunnel _cp _sp _                   = error "Can't make a call to myself!"

-- | Inductive case: the current node is attached to the next node in the path.
instance {-# OVERLAPPABLE #-} (Tunnel client (ClientOf server), Node server) =>
         Tunnel client server where
  tunnel cp sp call = tunnel cp sp' $ ServerHop ep (encode call)
    where
      ep = resolveEndpoint $ endpoint sp
      sp' = Proxy :: Proxy (ClientOf server)

-- * Defining and calling servers

-- | A server node in the network.
class MonadBlob m => Node (m :: * -> *) where
  -- | The client to which this node is attached. Each node must be attached to
  --   exactly one client. This means that the attachment relation is not
  --   commutative: if @a@ is attached to @b@, then @b@ may send requests to
  --   @a@, but not the other way around.
  --   The attachments of nodes thus form a tree, rooted at the client.
  --   This is necessitated by the need for paths to be unique and unambiguous,
  --   a restriction that may or may not be possible to lift in the future.
  type ClientOf m :: * -> *

  -- | The location at which the node can be reached.
  endpoint :: Proxy m -> EndpointConfig

  -- | Perform a computation of the given node type.
  invoke :: m a -> IO a
