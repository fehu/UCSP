
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import AUCSP.Agent.NegotiationEnvironment.Controller
import AUCSP.Agent.NegotiatingAgent
import AgentSystem.Generic -- hiding (AgentOfRole)

import AUCSP.Agent.Run.Data


main = do let def = undefined :: NegotiationDef Float
          ctrl <- newNegotiationController
          groups <- createNegotiators ctrl Group $ defGroups def
          undefined


createNegotiators :: ( AgentSystem sys, AgentRole (RoleT r a)
                     , NegotiationRole r a, NegotiatorOfRole (RoleT r a)
                     , NegotiatorOfRoleRef r
                     , RoleRef (RoleT r a) ~ AgentRef (RoleResult (RoleT r a))) =>
                    -- (AgentSystem sys, NegotiationRole r a
                    --  , NegotiatorOfRole r, AgentRole (RoleT r a)
                    --  , RoleRef r ~ AgentRef (RoleResult (RoleT r a))  ) =>
                     sys -> r
                  -> [RequiredData r a]
                  -> IO [KnownAgent (RoleT r a)]
-- createNegotiators = undefined
createNegotiators sys r = mapM $ \rdata -> newKnownAgent (RoleT r) undefined
                                       <$> newAgentOfRole sys d (return rdata)
  where d = negotiatingAgentDescriptor r
