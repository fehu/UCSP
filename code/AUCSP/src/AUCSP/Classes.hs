-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Classes
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module AUCSP.Classes (

Discipline(..), Classroom(..), Requirement(..)

, ClassCore(..), Class(..), classDiscipline, classGroup, classProfessor
, classReferredAgent, classRefersAgent
, classDuration

, Day(..)

, module AUCSP.Classes.DiscreteTime

) where

import CSP.Coherence

import AUCSP.AgentsInterface
import AUCSP.AgentsInterface.KnownAgents
import AUCSP.NegotiationRoles
import AUCSP.Classes.DiscreteTime

import Data.Set (Set)
import Data.Ix (Ix)



-----------------------------------------------------------------------------

data Discipline  = Discipline  {  disciplineId              :: String
                               ,  disciplineMinutesPerWeek  :: Int
                               ,  disciplineRequirements    :: Set Requirement
                               }
                 deriving (Show, Eq, Ord)


newtype Requirement = Requirement String deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------

data Classroom = Classroom {
    classroomId :: String,
    classroomCapacity :: Int,
    classroomCapabilities :: Set Requirement
  }
  deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------

data ClassCore = ClassCore{ classDiscipline' :: Discipline
                          , classGroup'      :: KnownAgent Group
                          , classProfessor'  :: KnownAgent Professor
                          }
deriving instance (KnownAgentsConstraints) => Show ClassCore
deriving instance (KnownAgentsConstraints) => Eq   ClassCore
deriving instance (KnownAgentsConstraints) => Ord  ClassCore

classReferredAgent' :: (KnownAgentsConstraints) => ClassCore -> [SomeAgent]
classReferredAgent' cc = [ SomeAgent Group     $ classGroup' cc
                         , SomeAgent Professor $ classProfessor' cc]

data Class = Class { classCore       :: ClassCore
                   , classRoom       :: Classroom
                   , classDay        :: Day
                   , classBegins     :: SomeDiscreteTime
                   , classEnds       :: SomeDiscreteTime
                   }
deriving instance (KnownAgentsConstraints) => Show Class
deriving instance (KnownAgentsConstraints) => Eq   Class
deriving instance (KnownAgentsConstraints) => Ord  Class

classDiscipline = classDiscipline' . classCore
classGroup      = classGroup' . classCore
classProfessor  = classProfessor' . classCore

classReferredAgent :: (KnownAgentsConstraints) => Class -> [SomeAgent]
classReferredAgent = classReferredAgent' . classCore

classRefersAgent :: (KnownAgentsConstraints) => SomeAgent -> Class -> Bool
classRefersAgent a = elem a . classReferredAgent

classDuration :: Class -> Minutes
classDuration c = diffMinutes (classBegins c) (classEnds c)

instance (KnownAgentsConstraints) =>
  InformationPiece Class where informationType _ = "Class"

-----------------------------------------------------------------------------

data Day  =  Monday | Tuesday | Wednesday
          | Thursday | Friday | Saturday
  deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

-----------------------------------------------------------------------------