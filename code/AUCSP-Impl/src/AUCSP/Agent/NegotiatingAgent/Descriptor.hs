-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.NegotiatingAgent.Descriptor
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module AUCSP.Agent.NegotiatingAgent.Descriptor(

  SystemIntegration(..), RoleSystemIntegration
, NegotiationRole, NegotiationRoles

, RoleBehaviour(..), RoleBehaviourDef(..)
, negotiatingAgentDescriptor
, negotiatingGenericAgentDescriptor

) where

import Agent.Behavior
import AgentSystem.Role

import AUCSP.Agent.NegotiatingAgent.Roles

import Data.Map.Strict (Map)
import Data.Set (Set)

-----------------------------------------------------------------------------

class SystemIntegration s res where
  handleSystemMessages :: MessageHandling s res

type RoleSystemIntegration r = SystemIntegration (RoleState r) (RoleResult r)

-----------------------------------------------------------------------------

data RoleBehaviour r = RoleBehaviour {
    theRole :: r
  , handleNegotiation :: MessageHandling (RoleState r) (RoleResult r)
  , proaction         :: AgentAction (RoleState r) (RoleResult r)
  , initialContexts   :: IO ObligationsContext
                      -> IO (PreferencesContext Coherence)
                      -> IO (Contexts Coherence)
  , initialExtraState :: IO (StateExtra r)
  }

class RoleBehaviourDef r where
  roleBehaviour  ::      RoleBehaviour r
  roleBehaviour' :: r -> RoleBehaviour r
  roleBehaviour' _ = roleBehaviour

-----------------------------------------------------------------------------

type NegotiationRoles = (NegotiationRole Group, NegotiationRole Professor)

type NegotiationRole r =  ( AgentRole r
                          , NegotiatorConstraint r
                          , RoleResult r ~ ()
                          , RoleState  r ~ AgentState r
                          , RoleArgs   r ~ NegotiatorData r
                          , RoleSysArgs r ~ ScheduleInterface
                          , RoleRef    r ~ AgentRef'
                          , RoleSystemIntegration r
                          , Typeable r, Typeable Coherence, Num Coherence
                          )

negotiatingAgentDescriptor :: ( NegotiationRole r
                              , RoleSysArgs r ~ ScheduleInterface
                                ) =>
                              RoleBehaviour r -> GenericRoleDescriptor r
negotiatingAgentDescriptor b =
  genericRoleDescriptor (theRole b) ((return .) . negotiatingGenericAgentDescriptor b)


negotiatingGenericAgentDescriptor :: ( NegotiationRole r
                                     , RoleSysArgs r ~ ScheduleInterface
                                      ) =>
                               RoleBehaviour r
                            -> ScheduleInterface
                            -> NegotiatorData r
                            -> GenericAgentOfRoleDescriptor r
negotiatingGenericAgentDescriptor b i d = GenericAgentDescriptor{
    agName  = uniqueAgentName d
  , agDebug = debugAgent d
  , emptyResult = EmptyResult :: EmptyResult ()
  , messageHandling = combineMessageHandling handleSystemMessages
                                            (handleNegotiation b)
  , action = proaction b
  , initialState = do extra <- initialExtraState b
                      ctxs  <- initialContexts b (personalObligations d)
                                                 (personalPreferences d)
                      newAgentState i extra ctxs
  }

---------------------------------------------------------------------------
