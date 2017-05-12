{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers, TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
import Haste.App

-- | This node carries supposedly sensitive information, so only the 'Firewall'
--   node can talk to it.
newtype SecretServer a = S (Server a)
  deriving (Functor, Applicative, Monad, MonadIO)

secretComputation :: RemotePtr (SecretServer Int)
secretComputation = static (remote $ return 42)

-- | Requests to 'SecretServer' must be proxied via this node.
newtype Firewall a = F (Server a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadConc, MonadClient)

proxyComputation :: RemotePtr (Firewall Int)
proxyComputation = static (remote $ dispatch secretComputation)

instance Node SecretServer where
  type Allowed SecretServer client = client ~ Firewall
instance Mapping SecretServer a where
  invoke env (S m) = invokeServer env m

instance Node Firewall
instance Mapping Firewall a where
  invoke env (F m) = invokeServer env m

main = runApp
  [ start (Proxy :: Proxy SecretServer)
  , start (Proxy :: Proxy Firewall)
  ] $ do
  -- This would not type check, since we're trying to bypass the firewall:
  -- liftIO . print =<< dispatch secretComputation

  -- But this does:
  liftIO . print =<< dispatch proxyComputation
