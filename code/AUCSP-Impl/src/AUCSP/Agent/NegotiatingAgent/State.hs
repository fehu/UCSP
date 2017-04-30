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
{-# LANGUAGE FlexibleContexts #-}

module AUCSP.Agent.NegotiatingAgent.State (

  AgentState(..), StateExtra, newAgentState

, CandidateDetails(..), CandidateCreationDetails(..)
, ACandidate

, MutableKnownAgents(..), newMutableKnownAgents, mkKnownAgents
, addKnownAgents, addKnownAgents'

-- * Export

, module Export

) where

import CSP.Coherence.Candidate

import AUCSP.Contexts as Export
import AUCSP.Classes  as Export
import AUCSP.Classes.Generation
import AUCSP.NegotiationRoles as Export

import AgentSystem

import AUCSP.Agent.Predef0
import AUCSP.Agent.SharedSchedule.Interface as Export
import AUCSP.AgentsInterface.KnownAgents    as Export


import Data.Typeable
import Control.Monad (unless)

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Concurrent.STM


-----------------------------------------------------------------------------

data AgentState r = AgentState{
   contexts        :: Contexts Coherence,
   bestInternalCoh :: TVar Coherence,

   knownAgentsVars :: MutableKnownAgents,
   knownAgents'    :: KnownAgents,
   stateExtra      :: StateExtra r
 }

type family StateExtra r :: *

type ACandidate = Candidate Coherence CandidateDetails

data MutableKnownAgents = MutableKnownAgents{
   varKnownGroups     :: TVar (Set (KnownAgent Group)),
   varKnownProfessors :: TVar (Set (KnownAgent Professor))
 }

mkKnownAgents :: MutableKnownAgents -> KnownAgents
mkKnownAgents a = KnownAgents (readVar varKnownGroups)
                              (readVar varKnownProfessors)
  where readVar :: (MutableKnownAgents -> TVar (Set x)) -> IO [x]
        readVar f = fmap Set.toList . readTVarIO $ f a

newMutableKnownAgents = atomically $ do groups  <- newTVar Set.empty
                                        profs   <- newTVar Set.empty
                                        return $ MutableKnownAgents
                                                 groups profs

addKnownAgents :: (NegotiatorOfRole Group, NegotiatorOfRole Professor) =>
                  MutableKnownAgents -> [KnownAgent Group]
                                     -> [KnownAgent Professor] -> IO ()
addKnownAgents mKnown groups profs =
  do unless (null groups) $ addKnownAgents' mKnown varKnownGroups groups
     unless (null profs)  $ addKnownAgents' mKnown varKnownProfessors profs


addKnownAgents' mKnown selVar new  =
  atomically $ modifyTVar (selVar mKnown)
                          (Set.union (Set.fromList new))

-----------------------------------------------------------------------------

newAgentState :: (Num Coherence) => StateExtra r -> Contexts Coherence -> IO (AgentState r)
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

-----------------------------------------------------------------------------
