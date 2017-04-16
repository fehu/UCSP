-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.SharedSchedule
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

module AUCSP.Agent.SharedSchedule(

  createSharedSchedule

, SharedSchedule(..), Schedule(..)

, ScheduleInterface(tryPutCandidate) -- , HasCreator(..)
, PutCandidateResult(..)

, AgentRef'

) where

import AUCSP.Agent.SharedSchedule.Interface
import AUCSP.Agent.SharedSchedule.Internal
import AUCSP.Agent.SharedSchedule.Observer
import AgentSystem.Generic
import AUCSP.Classes

import Data.Set (Set)

-----------------------------------------------------------------------------

createSharedSchedule :: AgentSystem sys => sys
                                        -> SomeDiscreteTimeDescriptor
                                        -> Set Classroom
                                        -> Int
                                        -> IO [ScheduleInterface] -- SharedScheduleRefs

createSharedSchedule sys td rooms nInterfaces =
  -- observer <- newAgentOfRole sys

  undefined

-- data SharedScheduleRefs = SharedScheduleRefs{
--     sharedScheduleInterface :: [AgentRef']
--   , sharedScheduleHolders   :: [AgentRef']
--   , sharedScheduleObserver  :: AgentRef Schedule
--   }

-----------------------------------------------------------------------------
