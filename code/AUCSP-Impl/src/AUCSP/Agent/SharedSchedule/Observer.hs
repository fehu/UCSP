-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.SharedSchedule.Observer
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}

module AUCSP.Agent.SharedSchedule.Observer where

import AUCSP.Agent.Predef0
import AUCSP.Agent.SharedSchedule.Interface

import Data.Set (Set)
import Data.Map.Strict (Map)

import Control.Concurrent.STM (TVar)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- * State

newtype ScheduleCompleteness = ScheduleCompleteness
                              (TVar (Map (AgentRefOfRole Group) Bool))

-----------------------------------------------------------------------------
-- *Args

newtype SharedScheduleHolders = SharedScheduleHolders [AgentRef']
newtype TotalCoherenceThresholdFilter = TotalCoherenceThresholdFilter (Coherence -> Bool)

-----------------------------------------------------------------------------
-- * Messages Public

data ScheduleObserverDemand = DemandReset | DemandBetter
    deriving (Typeable, Show, Eq)

-----------------------------------------------------------------------------
-- * Role Definition


instance RoleName ScheduleObserver where roleName = show
instance AgentRole ScheduleObserver where
  type RoleResult ScheduleObserver = (Schedule, Coherence)
  type RoleState  ScheduleObserver = ScheduleCompleteness
  type RoleSysArgs ScheduleObserver = TotalCoherenceThresholdFilter
  type RoleArgs   ScheduleObserver = SharedScheduleHolders

-----------------------------------------------------------------------------
