-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.NegotiationEnvironment.Controller
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module AUCSP.Agent.NegotiationEnvironment.Controller(

  NegotiationController, ctrlKnownAgents
, newNegotiationController, newDefaultNegotiationController
, shareKnownAgents

, ControllerForRole(addNegotiators)

) where

import AgentSystem.Generic
import AUCSP.Agent.NegotiatingAgent
import AUCSP.Agent.NegotiationEnvironment.Integration

import Data.Typeable (Typeable)
import Data.Maybe (fromJust)

import qualified Data.List as List

import Control.Monad (forM, forM_, (<=<) )
import Control.Arrow ( (&&&), second )
import Control.Exception (SomeException)

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)

import Control.Concurrent.STM

-----------------------------------------------------------------------------

data NegotiationController = NegotiationController {
    _ctrlSystem             :: SimpleAgentSystem ()
  -- , _ctrlScheduleInterfaces :: TVar [ScheduleInterface]
  , _ctrlScheduleHolders  :: Map Classroom AgentRef'
  , _ctrlScheduleObserver :: AgentRefOfRole ScheduleObserver
  , _ctrlKnownAgents'     :: MutableKnownAgents
  , ctrlKnownAgents       :: KnownAgents
  }


-- newNegotiationController :: (AgentSystem sys) => IO sys -> IO NegotiationController
-- newDefaultNegotiationController :: IO NegotiationController

-- | shareKnownAgents and await all agents done
shareKnownAgents :: NegotiationController -> IO (Response Done)


_ctrlNextSchedule :: NegotiationController -> IO ScheduleInterface

-----------------------------------------------------------------------------

class ControllerForRole r where
  -- | Creates agents and adds them to underlying `MutableKnownAgents`.
  addNegotiators  :: NegotiationController -> r -> [NegotiatorData r]
                                           -> IO [KnownAgent r]
  listNegotiators :: NegotiationController -> r -> IO [KnownAgent r]

-- data ControllerRole = forall r . (NegotiationRole r, ControllerForRole r) =>
--                       ControllerRole r

-- instance RoleName ControllerRole where roleName (ControllerRole r) = roleName r
-- instance Eq ControllerRole where (==) = roleEq
-- instance Ord ControllerRole where compare = roleCmp

-----------------------------------------------------------------------------

instance AgentsManager NegotiationController where
  listAgents = listAgents . _ctrlSystem
  findAgent  = findAgent  . _ctrlSystem
instance AgentSystem NegotiationController where
  listAgentsByRole  = listAgentsByRole  . _ctrlSystem
  listAgentsOfRole  = listAgentsOfRole  . _ctrlSystem
  listAgentsOfRoles = listAgentsOfRoles . _ctrlSystem
  findAgentOfRole   = findAgentOfRole   . _ctrlSystem

-----------------------------------------------------------------------------

instance SystemRoleArgsProvider NegotiationController r ScheduleInterface

-----------------------------------------------------------------------------

instance -- (SystemArgsProvider NegotiationController) =>
  SystemAgentsCreation NegotiationController where
    
----------------------------------------------------------------------------
----------------------------------------------------------------------------

-- newNegotiationController createSys = do
--   sys <- createSys
--   mKnown <- newMutableKnownAgents
--   return $ NegotiationController (KnownAgentSystem sys) mKnown
--                                  (mkKnownAgents mKnown)
--
-- newDefaultNegotiationController = newNegotiationController newSimpleAgentSystem

shareKnownAgents sys = do
  groups <- knownGroups     $ ctrlKnownAgents sys
  profs  <- knownProfessors $ ctrlKnownAgents sys

  forM_ groups $ updateKnownAgents [] profs
  forM_ profs  $ updateKnownAgents groups []

  doneGs <- forM groups (flip ask Done . knownRef)
  donePs <- forM profs  (flip ask Done . knownRef)

  return . fmap (const Done) . sequence $ doneGs ++ donePs

updateKnownAgents groups profs = flip send (KnownAgentsUpdate groups profs)
                               . knownRef


--   cache <- fmap (KnownAgentCache . Map.fromList)
--         . forM (List.nub . concat $ Map.elems rMap)
--         $ \r'@(ControllerRole r) ->
--                 ((,) r' . KnownAgentCacheEntry r) <$> listNegotiators sys r
--
--   return undefined
--
-- data KnownAgentCacheEntry = forall r . (NegotiationRole r, ControllerForRole r) =>
--      KnownAgentCacheEntry r [KnownAgent r]
--
-- newtype KnownAgentCache = KnownAgentCache (Map ControllerRole KnownAgentCacheEntry)
--
-- getFromCache :: KnownAgentCache -> ControllerRole -> SomeKnonAge

----------------------------------------------------------------------------

_ctrlNextSchedule = undefined

----------------------------------------------------------------------------

-- instance (Typeable Coherence, Num Coherence, RoleBehaviourDef Group) =>
--   ControllerForRole Group where
--     addNegotiators = createAndAddNegotiators varKnownGroups
--     listNegotiators sys _ = knownGroups $ ctrlKnownAgents sys
--
-- instance (Typeable Coherence, Num Coherence, RoleBehaviourDef Professor) =>
--   ControllerForRole Professor where
--     addNegotiators = createAndAddNegotiators varKnownProfessors
--     listNegotiators sys _ = knownProfessors $ ctrlKnownAgents sys


-- createAndAddNegotiators :: ( Typeable Coherence, Num Coherence
--                            , NegotiationRole r, RoleBehaviourDef r
--                             ) =>
--                            (MutableKnownAgents -> TVar (Set (KnownAgent r)))
--                         -> NegotiationController -> r -> [NegotiatorData r]
--                         -> IO [KnownAgent r]
-- createAndAddNegotiators selVar sys r = remember <=< createNegotiators sys r
--   where remember new = addKnownAgents' (_ctrlKnownAgents' sys) selVar new
--                     >> return new



-- -- | Should fail if any agent already exsits
-- createNegotiators :: ( SystemAgentsCreation sys
--                      , SystemArgsProvider sys ScheduleInterface
--                      , NegotiationRole r, RoleBehaviourDef r
--                       ) =>
--                      sys -> r -> [NegotiatorData r] -> IO [KnownAgent r]
-- createNegotiators sys r = mapM $ \rdata -> newKnownAgent r (roleNegotiatorData rdata)
--                                        <$> newAgentOfRole sys d (return rdata)
--   where d = negotiatingAgentDescriptor $ roleBehaviour' r

----------------------------------------------------------------------------
