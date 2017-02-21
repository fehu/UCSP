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
, ReportBestCandidate(..), BestCandidateReport(..)

) where

import Agent.Generic

import AUCSP.Agent.NegotiatingAgent.NegotiationDefinition

import Data.Typeable (Typeable)

import Control.Concurrent.STM

-----------------------------------------------------------------------------

newtype KnownAgentsUpdate = KnownAgentsUpdate KnownAgents
instance Show KnownAgentsUpdate where show _ = "KnownAgentsUpdate"

data ReportBestCandidate = ReportBestCandidate deriving Show
data BestCandidateReport = forall a . Typeable a =>
      BestCandidateReport (Maybe (ACandidate a))
instance Show BestCandidateReport where show _ = "BestCandidateReport"


type instance ExpectedResponse ReportBestCandidate = BestCandidateReport

-- newtype NegotiationTimeUpdate = NegotiationTimeUpdate Int

-----------------------------------------------------------------------------

instance (Typeable a) => SystemIntegration (AgentState r a) NegotiationPartialResult where
  handleSystemMessages = MessageHandling {
    msgHandle = selectMessageHandler [
                mbHandle $ \i (KnownAgentsUpdate upd) -> updateKnownAgents i upd
              ]
  , msgRespond = selectResponse [
                 mbResp $ \i ReportBestCandidate -> fmap BestCandidateReport
                                                  . readTVarIO . bestCandidate
                                                  $ agentState i
               ]
  }

updateKnownAgents i upd =
  do let known = knownAgentsVars $ agentState i
     groupsUpd <- knownGroups upd
     profsUpd  <- knownProfessors upd
     atomically $ do modifyTVar (varKnownGroups known )
                                (++ groupsUpd)
                     modifyTVar (varKnownProfessors known )
                                (++ profsUpd)


-----------------------------------------------------------------------------
