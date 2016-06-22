{-# LANGUAGE UndecidableInstances,
             ScopedTypeVariables,
             TypeFamilies,
             DataKinds,
             TypeOperators,
             PolyKinds,
             FlexibleInstances,
             FlexibleContexts #-}
-- | Type-level routing of requests from clients to servers attached to a
--   network and back again.
module Routing
  ( Node (..)
  , Path, Tunnel
  , tunnel
  ) where
import Data.Proxy
import Haste.Binary -- for serialization
import Protocol

-- * Type-level lists

-- | Split a type-level list in its head and tail parts.
tuncons :: Proxy (x ': xs) -> (Proxy x, Proxy xs)
tuncons _ = (Proxy, Proxy)

-- * Network traversal

-- | The path from the given server to the given client.
--   The first element in the list is the server from which to trace a path to
--   the client.
type family Path (client :: * -> *) (m :: * -> *) where
  Path client client = '[]
  Path client m      = m ': Path client (ClientOf m)

-- | Nest a server call in zero or more server hop packets, as directed by the
--   given path.
class Tunnel (path :: [* -> *]) where
  tunnel :: Proxy path -- ^ The path to traverse
         -> ServerCall -- ^ Server call to route
         -> (Endpoint, ServerCall)

-- | Base case: client and server are one and the same.
instance Node x => Tunnel '[x] where
  tunnel path c = (endpoint $ fst $ tuncons path, c)

-- | Inductive case: the current node is attached to the next node in the path.
instance (client ~ ClientOf server, Node server, Tunnel (client ': path)) =>
         Tunnel (server ': client ': path) where
  tunnel path pkt = tunnel ttail $ ServerHop (endpoint thead) (encode pkt)
    where (thead, ttail) = tuncons path

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
  type ClientOf m :: * -> *

  -- | The location at which the node can be reached.
  endpoint :: Proxy m -> Endpoint
