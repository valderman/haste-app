{-# LANGUAGE UndecidableInstances,
             ScopedTypeVariables,
             TypeFamilies,
             DataKinds,
             TypeOperators,
             PolyKinds,
             FlexibleInstances,
             FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Type-level routing of requests from clients to servers attached to a
--   network and back again.
module Routing
  ( Node (..)
  , Remote
  , request
  ) where
import Data.Proxy

-- * Type-level lists

-- | Get the last element of a non-empty type-level list.
type family Last (xs :: [k]) :: k where
  Last '[x]      = x
  Last (x ': xs) = Last xs

-- * Network traversal

-- | The path from the given server to the given client.
--   The first element in the list is the server from which to trace a path to
--   the client.
type family Path (client :: * -> *) (m :: * -> *) where
  Path client client = '[client]
  Path client m      = m ': Path client (Client m)

-- | Traverse a path from client to server, passing a request from node to node
--   as we go along.
class Remote (path :: [* -> *]) where
  request_ :: ((x ': xs) ~ path)
           => Proxy path           -- ^ The path to traverse
           -> (String -> x String) -- ^ Function to call on the server
           -> String               -- ^ Data to be sent to function
           -> Last path String

-- | Base case: client and server are one and the same.
instance Remote '[x] where
  request_ _ r = r

-- | Inductive case: the current node is attached to the next node in the path.
instance (client ~ Client server, Node server, Remote (client ': path)) =>
         Remote (server ': client ': path) where
  request_ path r = request_ (ttail path) $ req r
    where
      ttail :: Proxy (x ': xs) -> Proxy xs
      ttail _ = undefined


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
  type Client m :: * -> *

  -- | Perform a request to this node from the client it is attached to.
  req :: (String -> m String) -> String -> Client m String

-- | Perform a request from a client to some connected node.
request :: forall client server path.
           ( client ~ Last (Path client server)
           , (server ': path) ~ Path client server
           , Remote (Path client server)
           )
         =>
           (String -> server String) -> String -> client String
request = request_ (undefined :: Proxy (Path client server))
