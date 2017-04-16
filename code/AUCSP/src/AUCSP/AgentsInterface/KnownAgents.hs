-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.AgentsInterface.KnownAgents
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module AUCSP.AgentsInterface.KnownAgents (

  KnownAgents(..),KnownAgentsConstraints

, module Export

) where

import AUCSP.AgentsInterface   as Export
import AUCSP.NegotiationRoles  as Export

-----------------------------------------------------------------------------


data KnownAgents = KnownAgents{
  knownGroups       :: IO [KnownAgent Group],
  knownProfessors   :: IO [KnownAgent Professor]
  -- knownClassrooms   :: IO [KnownAgent Classroom]
  }

type KnownAgentsConstraints = ( NegotiatorOfRole Group
                              , NegotiatorOfRole Professor
                              -- , NegotiatorOfRole Classroom
                              )
