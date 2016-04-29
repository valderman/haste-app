{-# LANGUAGE UndecidableInstances,
             ScopedTypeVariables,
             TypeFamilies,
             FlexibleContexts #-}
module Routing
  ( Node (..)
  , Remote
  , request
  ) where

-- * Type-level lists

-- | A singleton non-empty type list. Specialized to values of kind @* -> *@
--   to avoid having to pull in PolyKinds.
data One (x :: * -> *)

-- | A single cons node in a non-empty type list.
data Cons (x :: * -> *) (xs :: *) :: *

-- | Get the last element of a non-empty type-level list.
type family Last (xs :: *) :: * -> * where
  Last (One x)     = x
  Last (Cons x xs) = Last xs

-- | Get the first element of a non-empty type-level list.
type family Head (xs :: *) :: * -> * where
  Head (One x)     = x
  Head (Cons x xs) = x



-- * Network traversal

-- | The path from the given server to the given client.
type family Path (client :: * -> *) (m :: * -> *) where
  Path client client = One client
  Path client m      = Cons m (Path client (Client m))

-- | Traverse a path from client to server, passing a request from node to node
--   as we go along.
class Remote (path :: *) where
  request_ :: path -> (String -> Head path String) -> String -> Last path String

-- | Base case: client and server are one and the same.
instance Remote (One x) where
  request_ _ r = r

-- | Inductive case: the current node is attached to the next node in the path.
instance (Head path ~ Client server , Node server , Remote path) =>
         Remote (Cons server path) where
  request_ path r = request_ (ttail path) $ req r
    where
      ttail :: Cons x xs -> xs
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
request :: forall client server.
           ( client ~ Last (Path client server)
           , server ~ Head (Path client server)
           , Remote (Path client server)
           )
         =>
           (String -> server String) -> String -> client String
request = request_ (undefined :: Path client server)
