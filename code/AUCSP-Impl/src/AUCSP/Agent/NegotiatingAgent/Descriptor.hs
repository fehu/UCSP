-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.NegotiatingAgent.Descriptor
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module AUCSP.Agent.NegotiatingAgent.Descriptor(

  SystemIntegration(..), RoleSystemIntegration
, NegotiatorOfRole(..), NegotiationRole

, negotiatingAgentDescriptor
, negotiatingGenericAgentDescriptor

) where

import Agent.Behavior
import AgentSystem.Role

import AUCSP.Agent.NegotiatingAgent.Roles

import Data.Map.Strict (Map)
import Data.Set (Set)

-----------------------------------------------------------------------------

class SystemIntegration s res where
  handleSystemMessages :: MessageHandling s res

type RoleSystemIntegration r = SystemIntegration (RoleState r) (RoleResult r)

-----------------------------------------------------------------------------


type NegotiationRole r =  ( AgentRole r
                          , NegotiatorConstraint r
                          , RoleResult r ~ ()
                          , RoleState  r ~ AgentState r
                          , RoleArgs   r ~ RequiredData r
                          , RoleRef    r ~ AgentRef'
                          , RoleSystemIntegration r
                          , Typeable r, Typeable Coherence, Num Coherence
                          )

negotiatingAgentDescriptor :: NegotiationRole r =>
                              r -> GenericRoleDescriptor r
negotiatingAgentDescriptor =
  flip genericRoleDescriptor (return . negotiatingGenericAgentDescriptor)


negotiatingGenericAgentDescriptor :: NegotiationRole r =>
                               RequiredData r
                            -> GenericAgentOfRoleDescriptor r
negotiatingGenericAgentDescriptor d = GenericAgentDescriptor{
    agName  = uniqueAgentName d
  , agDebug = debugAgent d
  , emptyResult = EmptyResult :: EmptyResult ()
  , messageHandling = combineMessageHandling handleSystemMessages
                                            (handleNegotiation d)
  , action = proaction d
  , initialState = do extra <- initialExtraState d
                      ctxs  <- initialContexts d
                      newAgentState extra ctxs
  }

---------------------------------------------------------------------------
