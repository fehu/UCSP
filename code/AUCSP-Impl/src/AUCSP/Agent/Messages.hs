-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.Messages
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

-- {-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}

module AUCSP.Agent.Messages (

-- * Misc

  AgentRef', NegotiatorsConstraint
, SomeCandidate(..), candidateInfo'

, module Export, Typeable, SomeCoherence(..), getSomeCoherence

-- * Messages

, OpinionRequest(..), OpinionRequestType(..), OpinionResponse(..)
, TryPutCandidate(..), PutCandidateResult(..), putCanidadateConflicts

) where

import CSP.Coherence          as Export
import AUCSP.Classes          as Export
import AUCSP.Context          as Export
import AUCSP.NegotiationRoles as Export
import AUCSP.AgentsInterface  as Export
import AgentSystem.Generic    as Export

import AUCSP.Context.External (SomeCoherence(..), getSomeCoherence)
import Data.Typeable (Typeable)
import Data.Function (on)

import Data.Set (Set)
import qualified Data.Set as Set

-----------------------------------------------------------------------------

type AgentRef' = AgentRef ()

type NegotiatorsConstraint = ( NegotiatorOfRole Group, NegotiatorOfRole Professor
                             , RoleRef Group     ~ AgentRefOfRole Group
                             , RoleRef Professor ~ AgentRefOfRole Professor
                             )

-----------------------------------------------------------------------------

data SomeCandidate = forall a d . ( Typeable a, Typeable d
                                 , Show (Candidate a d)
                                 ) =>
   SomeCandidate (Candidate a d)

candidateInfo' :: SomeCandidate -> Information
candidateInfo' (SomeCandidate c) = candidateInfo c

instance Eq  SomeCandidate where (==)    = (==)    `on` candidateInfo'
instance Ord SomeCandidate where compare = compare `on` candidateInfo'
instance Show SomeCandidate where show (SomeCandidate c) = show c

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data OpinionRequestType = OpinionRequestInternal | OpinionRequestExternal

data OpinionRequest = OpinionRequest {
    opinionType   :: OpinionRequestType
  , opinionMode   :: ContextMode
  , opinionAssume :: Set Class
  } deriving Typeable

newtype OpinionResponse = OpinionResponse SomeCoherence deriving (Typeable, Show)

type instance ExpectedResponse OpinionRequest = OpinionResponse

-----------------------------------------------------------------------------

newtype TryPutCandidate = TryPutCandidate SomeCandidate deriving (Typeable, Show)

data PutCandidateResult = PutCandidateSuccess
                        | PutCandidateConflicts (Set SomeCandidate)
  deriving (Typeable, Show)

type instance ExpectedResponse TryPutCandidate = PutCandidateResult

putCanidadateConflicts :: PutCandidateResult -> Set SomeCandidate
putCanidadateConflicts (PutCandidateConflicts s) = s
putCanidadateConflicts _                        = Set.empty




-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
