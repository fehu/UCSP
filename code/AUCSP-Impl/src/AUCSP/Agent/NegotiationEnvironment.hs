-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.NegotiationEnvironment
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}

module AUCSP.Agent.NegotiationEnvironment(

  NegotiationEnvironment, newNegotiationEnvironment

-- , createNegotiatorsOfRole

) where

import AgentSystem.Generic hiding (AgentOfRole)
import AgentSystem                              as Export
import AUCSP.Agent.NegotiatingAgent             as Export


-----------------------------------------------------------------------------

type NegotiationEnvironment = SomeAgentSystem

newNegotiationEnvironment :: IO NegotiationEnvironment
newNegotiationEnvironment = undefined -- TODO  SomeAgentSystem <$> newSimpleAgentSystem

-----------------------------------------------------------------------------


-----------------------------------------------------------------------------
