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
                              (Map (AgentRefOfRole Group) (TVar Bool))

-----------------------------------------------------------------------------
-- *Args

newtype SharedScheduleHolders = SharedScheduleHolders [AgentRef']
newtype NegotiatingGroups = NegotiatingGroups (Set (AgentRefOfRole Group))
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
  type RoleArgs   ScheduleObserver = ( NegotiatingGroups
                                     , SharedScheduleHolders
                                     , TotalCoherenceThresholdFilter)

-----------------------------------------------------------------------------
