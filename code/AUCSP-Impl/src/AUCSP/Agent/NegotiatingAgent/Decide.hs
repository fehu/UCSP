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
--    * Assess coherence in Final mode.
--    * Fail with error
--    * Update class-core pool after new agenet became known.
data GroupDecision a = UseNextClassCore (ACandidate a)
                     | GenerateTimeRoom (ACandidate a)
                     | NegotiateOver    (ACandidate a)
                     | ConfirmCoherence (ACandidate a)
                     | UpdateClassCores
                     | NegotiationFailed                                          -- TODO: reason


data GroupDecider a =
  GroupDecider (RoleState (RoleT Group a) -> ACandidate a -> IO (GroupDecision a))

groupDecide :: GroupDecider a -> RoleState (RoleT Group a)
                              -> ACandidate a
                              -> IO (GroupDecision a)
groupDecide (GroupDecider f) = f

-----------------------------------------------------------------------------
