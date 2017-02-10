-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.AgentsInterface.RoleData
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module AUCSP.AgentsInterface.RoleData (

  Capabilities(GroupCapabilities, ProfessorCapabilities) -- , ClassroomCapabilities
, groupSize, groupNeedsDisciplines
, professorCanTeach, professorMinClasses, professorMaxClasses

, RoleData'(capabilities, knownAgents, knownSelf)

, module Export

) where

import AUCSP.NegotiationRoles             as Export
import AUCSP.Classes                      as Export
import AUCSP.AgentsInterface.KnownAgents  as Export

import Data.Set (Set)

-----------------------------------------------------------------------------

data family Capabilities (agentRole :: *) :: *

data instance Capabilities Group = GroupCapabilities {
  groupSize              :: Int,
  groupNeedsDisciplines  :: Set Discipline
  }

data instance Capabilities Professor = ProfessorCapabilities {
  professorCanTeach      :: Set Discipline,
  professorMinClasses    :: Int,
  professorMaxClasses    :: Int
  }

-----------------------------------------------------------------------------

data RoleData' r = RoleData' {
    capabilities :: Capabilities r
  , knownAgents  :: KnownAgents
  , knownSelf    :: KnownAgent r
  }

instance AgentOfRoleData Group     where type RoleData Group     = RoleData' Group
instance AgentOfRoleData Professor where type RoleData Professor = RoleData' Professor
