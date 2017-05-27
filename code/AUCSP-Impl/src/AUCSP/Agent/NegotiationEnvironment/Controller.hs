-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.NegotiationEnvironment.Controller
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module AUCSP.Agent.NegotiationEnvironment.Controller(

  NegotiationController, ctrlKnownAgents, ctrlScheduleObserver

, newNegotiationController
, setupSharedSchedule, TotalCoherenceThresholdFilter(..)

, createNegotiators
, shareKnownAgents

) where

import AgentSystem.Simple.Template
import AUCSP.Agent.NegotiatingAgent
import AUCSP.Agent.NegotiationEnvironment.Integration
import AUCSP.Agent.SharedSchedule

import Data.Typeable (Typeable)
import Data.Maybe (fromJust)

import qualified Data.List as List

import Control.Monad (forM, forM_, (<=<) )
import Control.Arrow ( (&&&), second )
import Control.Exception (SomeException)

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)

import Control.Concurrent.STM

-----------------------------------------------------------------------------

data NegotiationController = NegotiationController {
    _ctrlRegisters        :: RoleRegistersVar
  , _ctrlSchedule         :: TVar [ScheduleInterface]
  , _ctrlScheduleObserver :: TVar (AgentRefOfRole ScheduleObserver)
  , _ctrlKnownAgents      :: MutableKnownAgents
  , ctrlKnownAgents       :: KnownAgents
  , _ctrlDiscreteTimeD    :: SomeDiscreteTimeDescriptor
  , _ctrlTotalCohThresh   :: TotalCoherenceThresholdFilter
  }

ctrlScheduleObserver = readTVarIO . _ctrlScheduleObserver

-----------------------------------------------------------------------------

genAgentSystemInstances "NegotiationController" "_ctrlRegisters"

-----------------------------------------------------------------------------

instance AgentSystemArgsProvider NegotiationController SomeDiscreteTimeDescriptor
  where agentSysArgs = return . _ctrlDiscreteTimeD
instance AgentSystemArgsProvider NegotiationController TotalCoherenceThresholdFilter
  where agentSysArgs = return . _ctrlTotalCohThresh

----------------------------------------------------------------------------
----------------------------------------------------------------------------

newNegotiationController :: SomeDiscreteTimeDescriptor
                         -> TotalCoherenceThresholdFilter
                         -> IO NegotiationController
newNegotiationController td cf = do
  regs <- simpleRoleRegistersVar
  sch  <- newTVarIO $ error "SharedSchedule isn't set up"
  scho <- newTVarIO $ error "SharedSchedule isn't set up"
  mKnown <- newMutableKnownAgents

  return NegotiationController{
    _ctrlDiscreteTimeD    = td
  , _ctrlTotalCohThresh   = cf
  , _ctrlRegisters        = regs
  , _ctrlSchedule         = sch
  , _ctrlScheduleObserver = scho
  , _ctrlKnownAgents      = mKnown
  , ctrlKnownAgents       = mkKnownAgents mKnown
  }

----------------------------------------------------------------------------
----------------------------------------------------------------------------

setupSharedSchedule :: (Typeable Coherence, Show Coherence) =>
                       NegotiationController -> Set Classroom -> Int -> Bool
                    -> IO ()
setupSharedSchedule ctrl rooms nInterfaces debug = do
  (observer, interfaces) <- createSharedSchedule ctrl rooms nInterfaces debug
  atomically $ do writeTVar (_ctrlSchedule ctrl)         interfaces
                  writeTVar (_ctrlScheduleObserver ctrl) observer

----------------------------------------------------------------------------

instance AgentSystemArgsProvider NegotiationController ScheduleInterface
  where agentSysArgs sys = atomically $ do h:t <- readTVar $ _ctrlSchedule sys
                                           writeTVar (_ctrlSchedule sys)
                                                     (t ++ [h])
                                           return h

----------------------------------------------------------------------------

-- | Should fail if any agent already exsits
createNegotiators :: (NegotiationRole r, RoleBehaviourDef r) =>
                     NegotiationController -> r -> [NegotiatorData r] -> IO [KnownAgent r]
createNegotiators ctrl r rdataList =
  forM rdataList $ \rdata -> newKnownAgent r (roleNegotiatorData rdata)
                         <$> newAgentOfRole ctrl d (return rdata)
  where d = negotiatingAgentDescriptor $ roleBehaviour' r

----------------------------------------------------------------------------

-- | shareKnownAgents and await all agents done
shareKnownAgents :: NegotiationController -> IO (Response Done)

shareKnownAgents sys = do
  groups <- knownGroups     $ ctrlKnownAgents sys
  profs  <- knownProfessors $ ctrlKnownAgents sys

  forM_ groups $ updateKnownAgents [] profs
  forM_ profs  $ updateKnownAgents groups []

  doneGs <- forM groups (flip ask Done . knownRef)
  donePs <- forM profs  (flip ask Done . knownRef)

  return . fmap (const Done) . sequence $ doneGs ++ donePs

updateKnownAgents groups profs = flip send (KnownAgentsUpdate groups profs)
                               . knownRef


--   cache <- fmap (KnownAgentCache . Map.fromList)
--         . forM (List.nub . concat $ Map.elems rMap)
--         $ \r'@(ControllerRole r) ->
--                 ((,) r' . KnownAgentCacheEntry r) <$> listNegotiators sys r
--
--   return undefined
--
-- data KnownAgentCacheEntry = forall r . (NegotiationRole r, ControllerForRole r) =>
--      KnownAgentCacheEntry r [KnownAgent r]
--
-- newtype KnownAgentCache = KnownAgentCache (Map ControllerRole KnownAgentCacheEntry)
--
-- getFromCache :: KnownAgentCache -> ControllerRole -> SomeKnonAge

----------------------------------------------------------------------------
----------------------------------------------------------------------------
