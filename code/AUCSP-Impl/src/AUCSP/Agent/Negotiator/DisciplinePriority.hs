
-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.Negotiator.DisciplinePriority
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module AUCSP.Agent.Negotiator.DisciplinePriority(

  DisciplinePriorities(priorityDisciplines), assessDisciplinePriorities

) where

import AUCSP.Agent.NegotiatingAgent.Roles

import Data.Maybe (mapMaybe)

import Data.Map.Strict (Map)
import Data.Set (Set)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
