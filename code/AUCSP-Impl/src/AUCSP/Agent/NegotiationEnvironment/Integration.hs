-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.NegotiationEnvironment.Integration
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-- | Supports following messages:
--   * KnownAgentsUpdate
--   * ReportBestCandidate -> BestCandidateReport
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module AUCSP.Agent.NegotiationEnvironment.Integration(

  KnownAgentsUpdate(..)
, Done(..)

) where

import Agent.Generic

import AUCSP.Agent.NegotiatingAgent

import Data.Typeable (Typeable)

import qualified Data.Set as Set

import Control.Concurrent.STM

-----------------------------------------------------------------------------

instance SystemIntegration (AgentState r) () where
  handleSystemMessages = MessageHandling {
    msgHandle = selectMessageHandler [
                mbHandle $ \i (KnownAgentsUpdate groupsUpd profsUpd) ->
                                addKnownAgents (knownAgentsVars $ agentState i)
                                               groupsUpd profsUpd
              ]
  , msgRespond = selectResponse [
                 mbResp $ \_ Done -> respond Done
               ]
  }


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data KnownAgentsUpdate = KnownAgentsUpdate [KnownAgent Group]
                                           [KnownAgent Professor]
  deriving Show

-----------------------------------------------------------------------------

data Done = Done deriving Show
type instance ExpectedResponse Done = Done

-----------------------------------------------------------------------------
