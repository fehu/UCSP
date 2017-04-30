-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.SharedSchedule.Observer
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module AUCSP.Agent.SharedSchedule.Observer where

import AUCSP.Agent.Predef0
import AUCSP.Agent.SharedSchedule.Interface
-- import AUCSP.Agent.SharedSchedule.Internal

import Data.Maybe (fromMaybe)
import Control.Monad ( when )

import Data.Set (Set)
import Data.Map.Strict (Map)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Control.Concurrent.STM

-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
-- * State and Args

newtype ScheduleCompleteness = ScheduleCompleteness
                              (Map (AgentRefOfRole Group) (TVar Bool))


newtype SharedScheduleHolders = SharedScheduleHolders [AgentRef']
newtype NegotiatingGroups = NegotiatingGroups (Set (AgentRefOfRole Group))
listNegotiatingGroups (NegotiatingGroups gs) = Set.toList gs

newtype TotalCoherenceThresholdFilter = TotalCoherenceThresholdFilter (Coherence -> Bool)
applyTotalCoherenceThreshold :: TotalCoherenceThresholdFilter -> Coherence -> Bool
applyTotalCoherenceThreshold (TotalCoherenceThresholdFilter f) = f


-----------------------------------------------------------------------------
-- * Messages Public

data ScheduleObserverDemand = DemandReset | DemandBetter
    deriving (Typeable, Show, Eq)

-----------------------------------------------------------------------------
-- * Implementation: ScheduleObserver


instance RoleName ScheduleObserver where roleName = show
instance AgentRole ScheduleObserver where
  type RoleResult ScheduleObserver = (Schedule, Coherence)
  type RoleState  ScheduleObserver = ScheduleCompleteness
  type RoleArgs   ScheduleObserver = ( NegotiatingGroups
                                     , SharedScheduleHolders
                                     , TotalCoherenceThresholdFilter)

scheduleObserverDescriptor :: (Typeable Coherence, NegotiatorsConstraint) =>
                        Bool -> GenericRoleDescriptor ScheduleObserver
scheduleObserverDescriptor debug =
   genericRoleDescriptor ScheduleObserver $
   \(gs, holders, threshold) -> return GenericAgentDescriptor{
        agName = "ScheduleObserver"
      , agDebug = debug
      , initialState = newScheduleCompleteness gs
      , messageHandling = MessageHandling{
            msgHandle = selectMessageHandler [
                  mbHandle $ \i (CandidatesChanges changes) -> atomically $
                             updateScheduleCompleteness (agentState i) changes
                ]
          , msgRespond = selectResponse []
          }
      , action = AgentAction $ \i -> do
                    complete <- atomically . isScheduleComplete $ agentState i
                    when complete $ do
                        scheduleClasses <- listAndReset holders
                        coherence <- totalCoherence scheduleClasses gs
                        scheduleObserverDemand $
                          if applyTotalCoherenceThreshold threshold coherence
                            then DemandBetter else DemandReset
      , emptyResult = EmptyResult
      }

newScheduleCompleteness :: NegotiatingGroups -> IO ScheduleCompleteness
newScheduleCompleteness = fmap (ScheduleCompleteness . Map.fromList)
                        . mapM ((flip (,) <$> newTVarIO False <*>) . return)
                        . listNegotiatingGroups

updateScheduleCompleteness :: ScheduleCompleteness
                           -> Map (AgentRefOfRole Group) CandidateChange
                           -> STM ()
updateScheduleCompleteness (ScheduleCompleteness cs) changes =
  do let update = Map.intersection cs changes
     sequence_ $ do (ref, upd) <- Map.toList changes
                    let Just var = Map.lookup ref update
                    return . writeTVar var $ boolCandidateChange upd

isScheduleComplete :: ScheduleCompleteness -> STM Bool
isScheduleComplete (ScheduleCompleteness cs) = fmap and .
                                               mapM readTVar $ Map.elems cs


-- resetScheduleHolders :: SharedScheduleHolders -> IO ()
-- resetScheduleHolders (SharedScheduleHolders hs) =
--   mapM_ (`send` ScheduleHolderReset) hs

listAndReset :: NegotiatorsConstraint => SharedScheduleHolders -> IO (Set Class)
listAndReset (SharedScheduleHolders hs) =
    fmap (Set.unions . maybe [] (map scheduleHolderClasses))
  . waitResponses =<< mapM (`ask` ScheduleHolderListAndReset) hs

totalCoherence :: Set Class -> NegotiatingGroups -> IO Coherence
totalCoherence cs (NegotiatingGroups gs) = undefined


scheduleObserverDemand :: ScheduleObserverDemand -> IO ()
scheduleObserverDemand = undefined
