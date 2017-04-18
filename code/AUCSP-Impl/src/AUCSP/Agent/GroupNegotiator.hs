-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.GroupNegotiator
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module AUCSP.Agent.GroupNegotiator where

import AUCSP.Agent.NegotiatingAgent.NegotiationDefinition

import Data.Set (Set)



-----------------------------------------------------------------------------

type instance StateExtra (RoleT Group a) a = GroupExtraState a

data GroupExtraState a = GroupExtraState {
    classroomSet          :: Set Classroom
  , timeDescriptor        :: SomeDiscreteTimeDescriptor
  , groupRoleData         :: RoleData' Group
  , disiciplinePriorities :: DisciplinePriorities
  }

-----------------------------------------------------------------------------

groupRoleDescriptor :: NegotiationRole Group a =>
                       GenericRoleDescriptor (RoleT Group a)
groupRoleDescriptor = negotiatingAgentDescriptor Group


-- groupRequiredData :: RequiredData Group a
groupRequiredData name debug = RequiredData{
    uniqueAgentName   = name
  , debugAgent        = debug
  , handleNegotiation = undefined
  , proaction         = undefined
  , initialContexts   = undefined
  , roleRequiredData  = undefined
  , initialExtraState = undefined
  }



-----------------------------------------------------------------------------

-- group


-----------------------------------------------------------------------------
