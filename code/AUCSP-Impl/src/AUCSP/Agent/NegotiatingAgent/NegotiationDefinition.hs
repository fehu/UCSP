-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.NegotiatingAgent.NegotiationDefinition
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}


module AUCSP.Agent.NegotiatingAgent.NegotiationDefinition(


  newKnownGroup, newKnownProfessor

, SystemIntegration(..), RoleSystemIntegration
, NegotiatorOfRole(..), NegotiationRole

-- * Discipline Priorities
, DisciplinePriorities(priorityDisciplines), assessDisciplinePriorities


-- * Negotiator Descriptor
, RequiredData(..), negotiatingAgentDescriptor
, negotiatingGenericAgentDescriptor

-- , GroupExtraState(..)

-- * Re-export

, module Export

) where

import AUCSP.Agent.Messages               as Export
import AUCSP.AgentsInterface.RoleData     as Export
import AUCSP.Agent.NegotiatingAgent.State as Export
import AUCSP.Agent.SharedSchedule         as Export

import Data.Maybe (mapMaybe)

import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-----------------------------------------------------------------------------


instance RoleName Group where roleName = show
instance AgentRole (RoleT Group a) where
  type RoleResult (RoleT Group a) = ()
  type RoleState  (RoleT Group a) = AgentState Group a
  type RoleArgs   (RoleT Group a) = RequiredData Group a


instance RoleName Professor where roleName = show
instance AgentRole (RoleT Professor a) where
  type RoleResult (RoleT Professor a) = ()
  type RoleState  (RoleT Professor a) = AgentState Professor a
  type RoleArgs   (RoleT Professor a) = RequiredData Professor a

-----------------------------------------------------------------------------

instance NegotiatorOfRoleRef Group where type RoleRef Group = AgentRef'
instance NegotiatorOfRole Group where
  data KnownAgent Group = KnownGroup AgentRef' (RoleData Group)
  roleOf _ = Group
  knownData (KnownGroup _ d) = d
  knownRef  (KnownGroup r _) = r
  knownAgentId = rawAgentId . agentId . knownRef
instance Show (KnownAgent Group) where
  show = ("KnownGroup " ++) . show . knownAgentId


instance NegotiatorOfRoleRef Professor where
  type RoleRef Professor = AgentRef'
instance NegotiatorOfRole Professor where
  data KnownAgent Professor = KnownProfessor Professor
                                             AgentRef'
                                            (RoleData Professor)

  roleOf   (KnownProfessor p _ _) = p
  knownData (KnownProfessor _ _ d) = d
  knownRef  (KnownProfessor _ r _) = r
  knownAgentId = rawAgentId . agentId . knownRef
instance Show (KnownAgent Professor) where
  show = ("KnownProfessor " ++) . show . knownAgentId


newKnownGroup = KnownGroup
newKnownProfessor = KnownProfessor


-----------------------------------------------------------------------------

-- type instance StateExtra (RoleT Group a) a = GroupExtraState a
--
-- data GroupExtraState a = GroupExtraState {
--     classroomSet          :: Set Classroom
--   , timeDescriptor        :: SomeDiscreteTimeDescriptor
--   , groupRoleData         :: RoleData' Group
--   , disiciplinePriorities :: DisciplinePriorities
--   }

-----------------------------------------------------------------------------

-- type instance StateExtra (RoleT Professor a) a = ()

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

class SystemIntegration s res where
  handleSystemMessages :: MessageHandling s res

type RoleSystemIntegration r = SystemIntegration (RoleState r) (RoleResult r)


data RequiredData r a = RequiredData {
    uniqueAgentName   :: String
  , debugAgent        :: Bool
  , handleNegotiation :: MessageHandling (RoleState (RoleT r a)) (RoleResult (RoleT r a))
  , proaction         :: AgentAction (RoleState (RoleT r a)) (RoleResult (RoleT r a))
  , initialContexts   :: IO (Contexts a)
  , roleRequiredData  :: RoleData (RoleT r a)
  , initialExtraState :: IO (StateExtra (RoleT r a) a)
  }

-----------------------------------------------------------------------------


type NegotiationRole r a =  ( RoleResult (RoleT r a)   ~ ()
                            , RoleState (RoleT r a)    ~ AgentState r a
                            , RoleArgs (RoleT r a)     ~ RequiredData r a
                            , StateExtra (RoleT r a) a ~ StateExtra r a
                            , RoleSystemIntegration (RoleT r a)
                            , Typeable r, Typeable a, Num a
                            )

negotiatingAgentDescriptor :: NegotiationRole r a =>
                              r -> GenericRoleDescriptor (RoleT r a)
negotiatingAgentDescriptor r =
  genericRoleDescriptor (RoleT r) (return . negotiatingGenericAgentDescriptor)


negotiatingGenericAgentDescriptor :: NegotiationRole r a =>
                               RequiredData r a
                            -> GenericAgentOfRoleDescriptor (RoleT r a)
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


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

-- | Discipline priority is ratio: #groups inscribed / #profs can teach.
newtype DisciplinePriorities = DisciplinePriorities
      { priorityDisciplines :: Map Discipline Rational }

assessDisciplinePriorities :: Set Discipline
                           -> Map group (Set Discipline)
                           -> Map prof  (Set Discipline)
                           -> Rational
                           -> DisciplinePriorities
assessDisciplinePriorities disciplines groupsNeed profsTeach priorityThreshold =
  DisciplinePriorities . Map.fromList . mapMaybe priority
                       $ Set.toList disciplines
  where count d = length . filter (elem d) . Map.elems
        priority d = let cgroup = count d groupsNeed
                         cprof  = count d profsTeach
                         ratio  = toRational cgroup / toRational cprof
                     in if ratio >= priorityThreshold
                        then Just (d, ratio) else Nothing

-----------------------------------------------------------------------------
