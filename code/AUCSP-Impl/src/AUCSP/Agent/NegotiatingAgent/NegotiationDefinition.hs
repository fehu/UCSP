-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.NegotiatingAgent.NegotiationDefinition
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}


module AUCSP.Agent.NegotiatingAgent.NegotiationDefinition(


  newKnownGroup, newKnownProfessor

, SystemIntegration(..), RoleSystemIntegration
, NegotiatorOfRole(..), NegotiationRole, NegotiatorCreation

, negotiatingAgentDescriptor, negotiatingGenericAgentDescriptor

, module Export

) where

import AUCSP.Agent.Messages               as Export
import AUCSP.AgentsInterface.RoleData     as Export
import AUCSP.Agent.NegotiatingAgent.State as Export
import AUCSP.Agent.SharedSchedule         as Export

-----------------------------------------------------------------------------


instance RoleName Group where roleName = show
instance AgentRole (RoleT Group a) where
  type RoleResult (RoleT Group a) = ()
  type RoleState  (RoleT Group a) = AgentState Group a
  type RoleArgs   (RoleT Group a) = RequitedData Group a


instance RoleName Professor where roleName = show
instance AgentRole (RoleT Professor a) where
  type RoleResult (RoleT Professor a) = ()
  type RoleState  (RoleT Professor a) = AgentState Professor a
  type RoleArgs   (RoleT Professor a) = RequitedData Professor a

-----------------------------------------------------------------------------

instance NegotiatorOfRoleRef Group where type RoleRef Group = AgentRef'
instance NegotiatorOfRole Group where
  data KnownAgent Group = KnownGroup AgentRef' (RoleData Group)
  roleOf _ = Group
  knownData (KnownGroup _ d) = d
  knownRef  (KnownGroup r _) = r
  knownAgentId = rawAgentId . agentId . knownRef
instance Show (KnownAgent Group) where
  show = ("KnownGroup " ++) . show . knownAgentId


instance NegotiatorOfRoleRef Professor where
  type RoleRef Professor = AgentRef'
instance NegotiatorOfRole Professor where
  data KnownAgent Professor = KnownProfessor Professor
                                             AgentRef'
                                            (RoleData Professor)

  roleOf   (KnownProfessor p _ _) = p
  knownData (KnownProfessor _ _ d) = d
  knownRef  (KnownProfessor _ r _) = r
  knownAgentId = rawAgentId . agentId . knownRef
instance Show (KnownAgent Professor) where
  show = ("KnownProfessor " ++) . show . knownAgentId


newKnownGroup = KnownGroup
newKnownProfessor = KnownProfessor

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class SystemIntegration s res where
  handleSystemMessages :: MessageHandling s res

type RoleSystemIntegration r = SystemIntegration (RoleState r) (RoleResult r)

class NegotiatorOfRole r => NegotiatorOfRoleCreation r where
  data RequitedData r :: * -> *

  uniqueAgentName   :: RequitedData r a -> String
  debugAgent        :: RequitedData r a -> Bool
  handleNegotiation :: RequitedData r a -> MessageHandling (RoleState r) (RoleResult r)
  proaction         :: RequitedData r a -> AgentAction (RoleState r) (RoleResult r)
  initialContexts   :: RequitedData r a -> IO (Contexts a)
  roleRequiredData  :: RequitedData r a -> RoleData r
  initialExtraState :: RequitedData r a -> IO (StateExtra r a)


-----------------------------------------------------------------------------


type NegotiationRole r a =  ( RoleResult r ~ ()
                            , RoleState r ~ AgentState r a
                            , RoleArgs r ~ RequitedData r a )

type NegotiatorCreation r a = ( NegotiationRole r a, Typeable r, Typeable a
                              , NegotiatorOfRoleCreation r
                              , RoleSystemIntegration r)

negotiatingAgentDescriptor :: ( NegotiatorCreation r a, Num a ) =>
                              r -> GenericRoleDescriptor r
negotiatingAgentDescriptor r =
  AgentRoleDescriptor r (return . negotiatingGenericAgentDescriptor)


negotiatingGenericAgentDescriptor :: ( NegotiatorCreation r a, Num a ) =>
                            RequitedData r a -> GenericAgentOfRoleDescriptor r
negotiatingGenericAgentDescriptor d = GenericAgentDescriptor{
    agName  = uniqueAgentName d
  , agDebug = debugAgent d
  , emptyResult = EmptyResult :: EmptyResult ()
  , messageHandling = combineMessageHandling handleSystemMessages
                                            (handleNegotiation d)
  , action = proaction d
  , initialState = do extra <- initialExtraState d
                      ctxs  <- initialContexts d
                      newAgentState extra ctxs
  }


-----------------------------------------------------------------------------
