{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-|
Module      : Servant.Polysemy.Client
Copyright   : (c) 2020 Alex Chapman
License     : BSD3
Maintainer  : alex@farfromthere.net
Stability   : experimental
Portability : GHC
Description : A Polysemy effect for running Servant commands (ClientM).

This module allows you to act as a client of a Servant API, within a Polysemy 'Sem'.
Use the servant-client package to generate your clients, which return in the 'ClientM' monad.
You can then use 'runClient' (or 'runClientStreaming') to run your client in 'Sem', and 'runServantClient' (or 'runServantClientStreaming') to interpret the effect.

See <example/Client.hs> for a simple example that can interact with the example servers in the same directory.
-}
module Servant.Polysemy.Client
  (
  -- * Effects
    ServantClient
  , runClient'
  , runClient

  -- * Interpreters
  , runServantClientWithEnv
  , runServantClientUrl
  , runServantClient

  -- * Re-exported from Servant
  , ClientError
  ) where

import Control.Monad ((>=>))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Polysemy
import Polysemy.Error
import Servant.Client
       ( BaseUrl
       , ClientEnv
       , ClientError
       , ClientM
       , mkClientEnv
       , parseBaseUrl
       , runClientM
       )

-- | The 'ServantClient' effect allows you to run a 'ClientM' as automatically generated for your API by the servant-client package.
data ServantClient m a where
  RunClient' :: ClientM o -> ServantClient m (Either ClientError o)

makeSem ''ServantClient

-- | Run this 'ClientM' in the 'Sem' monad.
runClient
  :: (Members '[ServantClient, Error ClientError] r)
  => ClientM o -> Sem r o
runClient = runClient' >=> fromEither

-- | Interpret the 'ServantClient' effect by running any calls to 'RunClient'' against the given 'BaseUrl' and 'ClientEnv'.
runServantClientWithEnv
  :: Member (Embed IO) r
  => ClientEnv
  -> Sem (ServantClient ': r) a -> Sem r a
runServantClientWithEnv env m =
  interpret (\(RunClient' client) -> embed $ runClientM client env) m

-- | Interpret the 'ServantClient' effect by running any calls to 'RunClient'' against the given 'BaseUrl'.
runServantClientUrl
  :: Member (Embed IO) r
  => BaseUrl -> Sem (ServantClient ': r) a -> Sem r a
runServantClientUrl server m = do
  manager <- embed $ newManager tlsManagerSettings
  let env = mkClientEnv manager server
  runServantClientWithEnv env m

-- | Parse the given string as a URL and then behave as 'runServantClientUrl' does.
runServantClient
  :: Member (Embed IO) r
  => String -> Sem (ServantClient ': r) a -> Sem r a
runServantClient server m = do
  server' <- embed $ parseBaseUrl server
  runServantClientUrl server' m
