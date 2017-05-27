-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.SharedSchedule.Internal.ObserverImpl
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module AUCSP.Agent.SharedSchedule.Internal.ObserverImpl where

import AUCSP.Agent.Predef0
import AUCSP.Agent.SharedSchedule.Interface
import AUCSP.Agent.SharedSchedule.Observer
import AUCSP.Agent.SharedSchedule.Internal.Messages

import Data.Maybe (fromMaybe)
import Control.Monad ( when )

import Data.Set (Set)
import Data.Map.Strict (Map)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Control.Concurrent.STM

-----------------------------------------------------------------------------
-- * Implementation: ScheduleObserver

scheduleObserverDescriptor :: (Typeable Coherence, NegotiatorsConstraint) =>
                        Bool -> GenericRoleDescriptor ScheduleObserver
scheduleObserverDescriptor debug =
   genericRoleDescriptor ScheduleObserver $
   \threshold holders -> return GenericAgentDescriptor{
        agName = "ScheduleObserver"
      , agDebug = debug
      , initialState = newScheduleCompleteness
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
                        coherence <- totalCoherence scheduleClasses
                        scheduleObserverDemand $
                          if applyTotalCoherenceThreshold threshold coherence
                            then DemandBetter else DemandReset
      , emptyResult = EmptyResult
      }

applyTotalCoherenceThreshold :: TotalCoherenceThresholdFilter -> Coherence -> Bool
applyTotalCoherenceThreshold (TotalCoherenceThresholdFilter f) = f


newScheduleCompleteness :: IO ScheduleCompleteness
newScheduleCompleteness = ScheduleCompleteness <$> newTVarIO Map.empty
                        --   fmap (ScheduleCompleteness . Map.fromList)
                        -- . mapM ((flip (,) <$> newTVarIO False <*>) . return)
                        -- . listNegotiatingGroups

updateScheduleCompleteness :: ScheduleCompleteness
                           -> Map (AgentRefOfRole Group) CandidateChange
                           -> STM ()
updateScheduleCompleteness (ScheduleCompleteness csVar) changes =
  csVar `modifyTVar` \m -> foldr (uncurry Map.insert) m
                         . Map.assocs $ fmap boolCandidateChange changes

isScheduleComplete :: ScheduleCompleteness -> STM Bool
isScheduleComplete (ScheduleCompleteness cs) = fmap (and . Map.elems)
                                                    (readTVar cs)


-- resetScheduleHolders :: SharedScheduleHolders -> IO ()
-- resetScheduleHolders (SharedScheduleHolders hs) =
--   mapM_ (`send` ScheduleHolderReset) hs

listAndReset :: NegotiatorsConstraint => SharedScheduleHolders -> IO (Set Class)
listAndReset (SharedScheduleHolders hs) =
    fmap (Set.unions . maybe [] (map scheduleHolderClasses))
  . waitResponse . sequence =<< mapM (`ask` ScheduleHolderListAndReset) hs

totalCoherence :: Set Class -> IO Coherence
totalCoherence cs = undefined


scheduleObserverDemand :: ScheduleObserverDemand -> IO ()
scheduleObserverDemand = undefined
