-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.NegotiatingAgent
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module AUCSP.Agent.NegotiatingAgent (
-- * Re-export

  module Export

-- hiding AgentSystem.Generic (AgentOfRole)

, AgentRole(..), RoleName(..), RoleT(..)
, SomeRole(..)

, AgentRefOfRole

, AgentRoleDescriptor(..), unsafeModifyAgentRoleDescriptor

, CreateAgentOfRole(..)

) where


import AgentSystem.Generic hiding (AgentOfRole)

import Agent.Generic                  as Export
import AgentSystem.Manager            as Export

import AUCSP.NegotiationRoles         as Export
import AUCSP.AgentsInterface          as Export
import AUCSP.AgentsInterface.RoleData as Export
import AUCSP.Classes                  as Export
import AUCSP.Contexts                 as Export

import AUCSP.Agent.NegotiatingAgent.State                 as Export
import AUCSP.Agent.NegotiatingAgent.NegotiationDefinition as Export
-- import AUCSP.Agent.NegotiatingAgent.SearchCandidates      as Export
-- import AUCSP.Agent.NegotiatingAgent.Decide                as Export
-- import AUCSP.Agent.NegotiatingAgent.HandleNegotiation     as Export

-----------------------------------------------------------------------------

-- instance DataForRole (RoleT Group a)                                         -- TODO

-----------------------------------------------------------------------------
