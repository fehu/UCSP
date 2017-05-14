-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.Negotiator.Group
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module AUCSP.Agent.Negotiator.Group where

import AUCSP.Agent.NegotiatingAgent
import AUCSP.AgentsInterface.RoleData

import Data.Set (Set)



-----------------------------------------------------------------------------

type instance StateExtra Group = GroupExtraState

data GroupExtraState = GroupExtraState {
    classroomSet          :: Set Classroom
  , timeDescriptor        :: SomeDiscreteTimeDescriptor
  , groupRoleData         :: RoleData' Group
  , disiciplinePriorities :: DisciplinePriorities
  }

-----------------------------------------------------------------------------

groupRoleDescriptor :: NegotiationRole Group => GenericRoleDescriptor Group
groupRoleDescriptor = negotiatingAgentDescriptor groupBehavior

groupBehavior :: RoleBehaviour Group
groupBehavior = RoleBehaviour{
    theRole = Group
  , handleNegotiation = undefined
  , proaction = undefined
  , initialContexts = undefined
  , initialExtraState = undefined
  }


-----------------------------------------------------------------------------

-- group


-----------------------------------------------------------------------------
