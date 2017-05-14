-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.Predef0
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module AUCSP.Agent.Predef0 (

  CoherenceSetup(..), CoherenceConstraints

, AgentRef', NegotiatorConstraint, NegotiatorsConstraint

, module Export, Typeable

) where

import AUCSP.Classes          as Export
import AUCSP.Contexts         as Export
import AUCSP.NegotiationRoles as Export
import AUCSP.AgentsInterface  as Export
import AgentSystem.Generic    as Export

import Data.Typeable

-----------------------------------------------------------------------------

class CoherenceSetup where type Coherence :: *

type CoherenceConstraints = ( Num Coherence, Ord Coherence
                            , Show Coherence, Typeable Coherence)

-----------------------------------------------------------------------------

type AgentRef' = AgentRef ()

type NegotiatorsConstraint = ( NegotiatorConstraint Group
                             , NegotiatorConstraint Professor)

type NegotiatorConstraint r = ( NegotiatorOfRole r
                              , RoleRef r ~ AgentRefOfRole r
                              )


-----------------------------------------------------------------------------
