-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.NegotiatingAgent.Decide
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module AUCSP.Agent.NegotiatingAgent.Decide(

  GroupDecision(..)
, GroupDecider(GroupDecider), groupDecide

) where

import AgentSystem.Role

import AUCSP.Agent.NegotiatingAgent.NegotiationDefinition

import Data.Typeable (Typeable)

-----------------------------------------------------------------------------

-- | Decisions to be made by Group agent:
--    * Try next class-core.
--    * Try another day-time-room configuration.
--    * Negotiate over a candidate.
--    * Fail with error
data GroupDecision = UseNextClassCore
                   | GenerateTimeRoom
                   | forall a . Typeable a => NegotiateOver (ACandidate a)
                   | NegotiationFailed                                          -- TODO: reason

data GroupDecider a =
  GroupDecider (RoleState Group -> ACandidate a -> IO GroupDecision)

groupDecide :: GroupDecider a -> RoleState Group -> ACandidate a -> IO GroupDecision
groupDecide (GroupDecider f) = f

-----------------------------------------------------------------------------
