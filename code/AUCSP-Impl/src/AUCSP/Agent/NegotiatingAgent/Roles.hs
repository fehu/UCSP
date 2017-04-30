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

module AUCSP.Agent.NegotiatingAgent.Roles(

  RequiredData(..) , module Export

) where

import AUCSP.Agent.Predef0                as Export
import AUCSP.Agent.NegotiatingAgent.State as Export

-----------------------------------------------------------------------------

data RequiredData r = RequiredData {
    uniqueAgentName   :: String
  , debugAgent        :: Bool
  , handleNegotiation :: MessageHandling (RoleState r) (RoleResult r)
  , proaction         :: AgentAction (RoleState r) (RoleResult r)
  , initialContexts   :: IO (Contexts Coherence)
  , roleRequiredData  :: RoleData r
  , initialExtraState :: IO (StateExtra r)
  }

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

instance RoleName Group where roleName = show
instance AgentRole Group where
  type RoleResult Group = ()
  type RoleState  Group = AgentState Group
  type RoleArgs   Group = RequiredData Group


instance RoleName Professor where roleName = show
instance AgentRole Professor where
  type RoleResult Professor = ()
  type RoleState  Professor = AgentState Professor
  type RoleArgs   Professor = RequiredData Professor

-----------------------------------------------------------------------------

instance NegotiatorOfRoleRef Group where type RoleRef Group = AgentRef'
instance NegotiatorOfRole Group where
  data KnownAgent Group = KnownGroup AgentRef' (RoleData Group)
  roleOf _ = Group
  knownData (KnownGroup _ d) = d
  knownRef  (KnownGroup r _) = r
  knownAgentId = rawAgentId . agentId . knownRef
  newKnownAgent _ = flip KnownGroup
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
  newKnownAgent r = flip $ KnownProfessor r
instance Show (KnownAgent Professor) where
  show = ("KnownProfessor " ++) . show . knownAgentId

-----------------------------------------------------------------------------
