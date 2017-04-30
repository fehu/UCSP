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
import AUCSP.Agent.SharedSchedule

import qualified Data.Set as Set

-----------------------------------------------------------------------------

data NegotiationDef = NegotiationDef{
    defDisciplines :: [Discipline]
  , defClassrooms  :: [Classroom]
  , defGroups      :: [NegotiatorData Group]
  , defProfessors  :: [NegotiatorData Professor]
  , defTimetable   :: SomeDiscreteTimeDescriptor
  , defTotalCoherenceFilter :: TotalCoherenceThresholdFilter
  }

createNegotiation :: ( Typeable Coherence, Num Coherence, Show Coherence
                     , RoleBehaviourDef Group, RoleBehaviourDef Professor
                      ) =>
           Bool -> NegotiationDef -> IO ( NegotiationController
                                        , AgentRefOfRole ScheduleObserver)

-----------------------------------------------------------------------------

createNegotiation debug def@NegotiationDef{ defGroups     = groups
                                          , defProfessors = profs
                                          , defTimetable  = tt
                                          } = do
  let rooms = Set.fromList $ defClassrooms def

  ctrl <- newDefaultNegotiationController

  (observer, _) <- createSharedSchedule ctrl tt rooms
                                        (defTotalCoherenceFilter def)
                                        undefined debug
  addNegotiators ctrl Group groups
  addNegotiators ctrl Professor profs
  shareKnownAgents ctrl >>= waitResponseSuccess
  return (ctrl, undefined)
