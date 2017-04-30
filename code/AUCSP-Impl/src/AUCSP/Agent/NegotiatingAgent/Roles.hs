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

  NegotiatorData(..) , module Export

) where

import AUCSP.Agent.Predef0                as Export
import AUCSP.Agent.NegotiatingAgent.State as Export
import AUCSP.Contexts

-----------------------------------------------------------------------------

data NegotiatorData r = NegotiatorData {
    uniqueAgentName     :: String
  , debugAgent          :: Bool
  , personalObligations :: IO ObligationsContext
  , personalPreferences :: IO (PreferencesContext Coherence)
  , roleNegotiatorData  :: RoleData r
  , initialExtraState   :: IO (StateExtra r)
  }

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

instance RoleName Group where roleName = show
instance AgentRole Group where
  type RoleResult Group = ()
  type RoleState  Group = AgentState Group
  type RoleArgs   Group = NegotiatorData Group


instance RoleName Professor where roleName = show
instance AgentRole Professor where
  type RoleResult Professor = ()
  type RoleState  Professor = AgentState Professor
  type RoleArgs   Professor = NegotiatorData Professor

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
