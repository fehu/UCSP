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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

module AUCSP.Agent.NegotiationEnvironment.Controller(

  NegotiationController, newNegotiationController
, negotiationSystem, notifyNegotiation
, startNegotiation, pauseNegotiation, terminateNegotiation
-- , createNegotiatorsOfRole

) where

import AgentSystem
import AgentSystem.Generic hiding (AgentOfRole)
import AUCSP.Agent.NegotiatingAgent.NegotiationDefinition
import AUCSP.Agent.NegotiationEnvironment.Integration
import AUCSP.Agent.SharedSchedule (Schedule)

import Data.Typeable (Typeable)
import Data.Maybe (fromJust)

import Control.Monad (forM, forM_, (<=<) )
import Control.Arrow ( (&&&), second )
import Control.Exception (SomeException)

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Concurrent.STM

-----------------------------------------------------------------------------

data NegotiationController = NegotiationController SomeAgentSystem MutableKnownAgents

newNegotiationController :: IO NegotiationController

negotiationSystem     :: NegotiationController -> SomeAgentSystem
startNegotiation      :: NegotiationController -> IO ()
pauseNegotiation      :: NegotiationController -> IO ()
terminateNegotiation  :: NegotiationController -> IO ()
notifyNegotiation     :: (Message msg) => NegotiationController -> msg -> IO ()


type NegotiationResult = Schedule

-- class (AgentRole (RoleT r a)) => NegotiatorsOfRoleCreation r a where
--   createNegotiatorsOfRole :: (NegotiationRole r a) =>
--                              NegotiationController
--                           -> GenericRoleDescriptor (RoleT r a)
--                           -> [IO (RequitedData r a)]
--                           -> IO [KnownAgent r]

----------------------------------------------------------------------------
----------------------------------------------------------------------------

newNegotiationController = NegotiationController
                        <$> (SomeAgentSystem <$> newSimpleAgentSystem)
                        <*> newMutableKnownAgents

negotiationSystem (NegotiationController sys _) = sys
startNegotiation      = startAllAgents . negotiationSystem
pauseNegotiation      = pauseAllAgents . negotiationSystem
terminateNegotiation  = terminateAllAgents . negotiationSystem
notifyNegotiation c msg = mapM_ (`send` msg) <=<
                          listAgents $ negotiationSystem c

----------------------------------------------------------------------------

type NegotiatorAgentsConstraints = ( KnownAgentsConstraints
                                   , NegotiatorOfRole Group
                                   , NegotiatorOfRole Professor
                                   , AgentRole Group
                                   , AgentRole Professor
                                   , Show     (RoleResult Group)
                                   , Show     (RoleResult Professor)
                                   , Typeable (RoleResult Group)
                                   , Typeable (RoleResult Professor) )

-- instance NegotiatorAgentsConstraints =>
--   NegotiatorsOfRole Group a where
--     createNegotiatorsOfRole = createNegotiatorsOfRole'
--                                 newKnownGroup varKnownGroups
--                                 ((`KnownAgents` return []) . return)
--                                 [ProfessorFullTime, ProfessorPartTime]
--
-- instance NegotiatorAgentsConstraints =>
--   NegotiatorsOfRole Professor a where
--     createNegotiatorsOfRole ctrl descr@(AgentRoleDescriptor (RoleT r) _) =
--        createNegotiatorsOfRole' (newKnownProfessor r)  varKnownProfessors
--                                  (KnownAgents (return []) . return)
--                                  [Group] ctrl descr


-- createNegotiatorsOfRole' newKnownAgent varKnownAgents newKnownAgents counterpartRoles
--   (NegotiationController sys globalKnown) roleD args =
--   do refs <- forM args $ newAgentOfRole sys roleD
--      newKnown <- sequence $
--                  do (ref, arg) <- refs `zip` args
--                     return $ (newKnownAgent ref . roleRequiredData) <$> arg
--      atomically $ varKnownAgents globalKnown `modifyTVar` (++ newKnown)
--
--      let upd = KnownAgentsUpdate $ newKnownAgents newKnown
--      counterparts <- sys `listAgentsOfRoles` counterpartRoles
--      forM_ counterparts (`sendPriority` upd)
--
--      return newKnown

-----------------------------------------------------------------------------
