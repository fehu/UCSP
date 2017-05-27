-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.NegotiationEnvironment
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module AUCSP.Agent.NegotiationEnvironment(

  NegotiationDef(..)

, module Export

) where

import AgentSystem.Generic                            as Export
import AUCSP.Agent.NegotiatingAgent                   as Export
import AUCSP.Agent.NegotiationEnvironment.Controller  as Export

import qualified Data.Set as Set

-----------------------------------------------------------------------------

data NegotiationDef = NegotiationDef{
    defDisciplines :: [Discipline]
  , defClassrooms  :: [Classroom]
  , defGroups      :: [NegotiatorData Group]
  , defProfessors  :: [NegotiatorData Professor]
  , defTimetable   :: SomeDiscreteTimeDescriptor
  , defTotalCoherenceFilter :: TotalCoherenceThresholdFilter
  , defScheduleInterfaces   :: Int
  }

-----------------------------------------------------------------------------

-- | 1. Creates negotiation controller
--   2. Sets up SharedSchedule
--   3. Creates Group and Professor negotiators
--   4. Shares KnownAgents between the negotiators
createNegotiation :: ( Typeable Coherence, Num Coherence, Show Coherence
                     , RoleBehaviourDef Group, RoleBehaviourDef Professor
                      ) =>
           Bool -> NegotiationDef -> IO NegotiationController

createNegotiation debug def@NegotiationDef{ defGroups     = groups
                                          , defProfessors = profs
                                          , defTimetable  = tt
                                          , defTotalCoherenceFilter = tcf
                                          , defScheduleInterfaces   = nInterface
                                          } = do
  let rooms = Set.fromList $ defClassrooms def

  ctrl <- newNegotiationController tt tcf
  setupSharedSchedule ctrl rooms nInterface debug
  createNegotiators ctrl Group groups
  createNegotiators ctrl Professor profs
  shareKnownAgents ctrl >>= waitResponseSuccess
  return ctrl
