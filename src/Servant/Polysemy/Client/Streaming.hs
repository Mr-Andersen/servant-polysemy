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
module Servant.Polysemy.Client.Streaming
  (
  -- * Effects
    ServantClientStreaming
  , runClientStreaming

  -- * Interpreters
  , runServantClientStreamingWithEnv
  , runServantClientStreamingUrl
  , runServantClientStreaming

  -- * Re-exported from Servant
  , ClientError
  ) where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Polysemy
import Polysemy.Cont
import Polysemy.Error
import Servant.Client.Streaming
       ( BaseUrl
       , ClientEnv
       , ClientError
       , ClientM
       , mkClientEnv
       , parseBaseUrl
       , withClientM
       )

-- | The 'ServantClientStreaming' effect is just like the 'ServantClient' effect,
-- but allows streaming connections.
data ServantClientStreaming m a where
  RunClientStreaming :: ClientM o -> ServantClientStreaming m o

makeSem ''ServantClientStreaming

-- | Interpret the 'ServantClient' effect by running any calls to 'RunClient'' against the given 'BaseUrl' and 'ClientEnv'.
runServantClientStreamingWithEnv
  :: Members
    '[ Cont ref
     , Embed IO
     , Error ClientError
     ] r
  => ClientEnv
  -> Sem (ServantClientStreaming ': r) a -> Sem r a
runServantClientStreamingWithEnv env m =
  interpret (\case
    RunClientStreaming client ->
      subst (\continue ->
        withLowerToIO $ \unliftIO _ ->
          withClientM client env (unliftIO . jump continue)
        ) fromEither
    ) m

-- | Interpret the 'ServantClientStreaming' effect by running any calls to 'RunClientStreaming' against the given URL.
-- Note that this adds a 'Cont' effect, which you can interpret using 'runContM', probably just before your call to 'runM'.
runServantClientStreamingUrl
  :: Members
    '[ Cont ref
     , Embed IO
     , Error ClientError
     ] r
  => BaseUrl -> Sem (ServantClientStreaming ': r) a -> Sem r a
runServantClientStreamingUrl server m = do
  manager <- embed $ newManager tlsManagerSettings
  let env = mkClientEnv manager server
  runServantClientStreamingWithEnv env m

-- | Parse the given string as a URL and then behave as 'runServantClientStreamingUrl'.
runServantClientStreaming
 :: Members
    '[ Cont ref
     , Embed IO
     , Error ClientError
     ] r
  => String -> Sem (ServantClientStreaming ': r) a -> Sem r a
runServantClientStreaming server m = do
  server' <- embed $ parseBaseUrl server
  runServantClientStreamingUrl server' m
