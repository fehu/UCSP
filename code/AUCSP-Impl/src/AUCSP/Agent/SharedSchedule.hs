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

, TotalCoherenceThresholdFilter(..)

) where

import AUCSP.Agent.NegotiatingAgent
import AUCSP.Agent.SharedSchedule.Observer

import AUCSP.Agent.SharedSchedule.Internal.ObserverImpl
import AUCSP.Agent.SharedSchedule.Internal.RoomScheduleHolder

import Data.Set (Set)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Control.Monad (forM)

-----------------------------------------------------------------------------

createSharedSchedule :: ( AgentSystem sys
                        , AgentSystemArgsProvider sys SomeDiscreteTimeDescriptor
                        , AgentSystemArgsProvider sys TotalCoherenceThresholdFilter
                        , Typeable Coherence, Show Coherence
                        ) =>
                         sys -> Set Classroom -> Int -> Bool
                     ->  IO (AgentRefOfRole ScheduleObserver, [ScheduleInterface])
createSharedSchedule sys rooms nInterfaces debug = do
  holders <- forM (Set.toList rooms)
           $ \r -> (,) r <$> newAgentOfRole sys (scheduleHolderDescriptor debug)
                                                (return r)
  observer <- newAgentOfRole sys (scheduleObserverDescriptor debug)
                                 (return . SharedScheduleHolders $ map snd holders)
  interfaces <- forM [1..nInterfaces] $
                \n -> newAgentOfRole sys (sharedScheduleDescriptor debug n)
                    $ return (Map.fromList holders, observer)
  return (observer, map newScheduleInterface interfaces)

-----------------------------------------------------------------------------
