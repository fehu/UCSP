-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.NegotiatingAgent.State
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------


{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AUCSP.Agent.NegotiatingAgent.State (

  AgentState(..), StateExtra, newAgentState
, SomeAgentState(..), ACandidate
, CandidateDetails(..), CandidateCreationDetails(..)

, MutableKnownAgents(..), newMutableKnownAgents

-- * Export

, Contexts(..)

) where

import CSP.Coherence.Candidate

import AUCSP.Contexts
import AUCSP.Classes
import AUCSP.Classes.Generation
import AUCSP.NegotiationRoles

import AgentSystem

import AUCSP.Agent.SharedSchedule


import Data.Typeable

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Concurrent.STM

-- import AUCSP.Agent.NegotiatingAgent.Decide

-----------------------------------------------------------------------------

data AgentState r a = AgentState{
   contexts        :: Contexts a,
   bestInternalCoh :: TVar a,

   knownAgentsVars :: MutableKnownAgents,
   knownAgents'    :: KnownAgents,
   stateExtra      :: StateExtra r a
 }

type family StateExtra r a :: *

type ACandidate a = Candidate a CandidateDetails

data SomeAgentState r = forall a . Typeable a => SomeAgentState (AgentState r a)

data MutableKnownAgents = MutableKnownAgents{
   varKnownGroups     :: TVar [KnownAgent Group],
   varKnownProfessors :: TVar [KnownAgent Professor]
 }

mkKnownAgents :: MutableKnownAgents -> KnownAgents
mkKnownAgents a = KnownAgents (readTVarIO $ varKnownGroups a)
                              (readTVarIO $ varKnownProfessors a)

newMutableKnownAgents = atomically $ do groups  <- newTVar []
                                        profs   <- newTVar []
                                        return $ MutableKnownAgents
                                                 groups profs

-----------------------------------------------------------------------------

newAgentState :: (Num a) => StateExtra r a -> Contexts a -> IO (AgentState r a)
newAgentState extra cxts = do bestVar   <- newTVarIO 0
                              knownVar  <- newMutableKnownAgents
                              return $ AgentState cxts bestVar knownVar
                                       (mkKnownAgents knownVar) extra


-----------------------------------------------------------------------------

data CandidateDetails = CandidateDetails CandidateCreationDetails
                                         CandidateCreator
  deriving Show


newtype CandidateCreationDetails = CandidateCreationDetails SomeClassesGenerator
instance Show CandidateCreationDetails where show _ = "CandidateCreationDetails"

type CandidateCreator = AgentRefOfRole Group

-- instance (RoleResult Group ~ ()) => HasCreator CandidateDetails AgentRef' where
--   getCreator (CandidateDetails _ c) = c

-----------------------------------------------------------------------------
