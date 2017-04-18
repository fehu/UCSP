-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.SharedSchedule
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module AUCSP.Agent.SharedSchedule(

  createSharedSchedule

, Schedule(..)
, ScheduleInterface(tryPutCandidate)

-- * Public Roles
, SharedSchedule(..)
, ScheduleObserver(..)

-- * Constructor args

, NegotiatingGroups(..)
, TotalCoherenceThresholdFilter(..)

) where

import AUCSP.Agent.Messages
import AUCSP.Agent.SharedSchedule.Interface
import AUCSP.Agent.SharedSchedule.Internal
import AUCSP.Agent.SharedSchedule.Observer

import Data.Set (Set)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Control.Monad (forM)

-----------------------------------------------------------------------------

createSharedSchedule :: ( AgentSystem sys, NegotiatorsConstraint
                        , Typeable a, Show a
                        , RoleResult ScheduleObserver ~ (Schedule, a)
                        ) =>
                        sys
                     -> SomeDiscreteTimeDescriptor
                     -> Set Classroom
                     -> NegotiatingGroups
                     -> TotalCoherenceThresholdFilter a
                     -> Int -> Bool
                     -> IO [ScheduleInterface]

createSharedSchedule sys td rooms groups totalFilter nInterfaces debug = do
  holders <- forM (Set.toList rooms) $
              \room -> (,) room <$>
                       newAgentOfRole sys (scheduleHolderDescriptor debug)
                                          (return (td, room))
  observer <- newAgentOfRole sys (scheduleObserverDescriptor debug)
            $ return ( groups, SharedScheduleHolders $ map snd holders, totalFilter )
  interfaces <- forM [1..nInterfaces] $
                 \n -> newAgentOfRole sys (sharedScheduleDescriptor debug n)
                     $ return (Map.fromList holders, observer)
  return $ map newScheduleInterface interfaces

-- data SharedScheduleRefs = SharedScheduleRefs{
--     sharedScheduleInterface :: [AgentRef']
--   , sharedScheduleHolders   :: [AgentRef']
--   , sharedScheduleObserver  :: AgentRef Schedule
--   }

-----------------------------------------------------------------------------
