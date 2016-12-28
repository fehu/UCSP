
%if False
\begin{code}

module AUCSP.Context.Capabilities(

  Capabilities(GroupCapabilities, FullTimeProfCapabilities, ClassroomCapabilities)

, CanTeachRel(..), NeedsDisciplineRel(..)
, MeetsRequirementsRel(..), EnoughCapacityRel(..)
) where

import Agent.Abstract

import AUCSP.Classes
import qualified AUCSP.NegotiationRoles as Role
import AUCSP.Coherence
import AUCSP.Context

import qualified AUCSP.Context.Combine as Combine

import Data.Coerce (coerce)
import Data.Set (member)
import Data.Typeable (Typeable)

import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Applicative ((<|>))

\end{code}
%endif

\subsubsection{Capabilities}
The capabilities context handles question ``Am I able to do it?''.
It's main purpose is to discard immediately any proposal that
would never be accepted.

\begin{itemize}
  \item \emph{Group}: ``Am I interested in the discipline?''
  \item \emph{Professor}: ``Am I qualified to teach the disciple?''
  \item \emph{Classroom}: ``Do I suit the disciple?'',
                          ``Do I have the capacity required?''
\end{itemize}

\noindent
An agent should mark any other agent, that has declined some proposal for
\emph{capabilities} reasons, describing the reason. It should
further avoid making same kind of proposals to the uncapable agent.

\begin{figure}[h]
  \centering
  \input{Document/tikz/Capabilities}
  \caption{Capabilities required to form a \emph{class}.}
  \label{fig:capabilities}
\end{figure}

\begin{code}

data family Capabilities (agentRole :: *) :: * -> *

data instance Capabilities Role.Group a = GroupCapabilities {
  groupSize         :: Int,
  needsDisciplines  :: [Discipline]
  }

data instance Capabilities Role.Professor a = FullTimeProfCapabilities {
  canTeachFullTime :: [Discipline]
  }

data instance Capabilities Role.Classroom a = ClassroomCapabilities {
  classroomCapacity      :: Int,
  classroomCapabilities  :: [Requirement]
  }

-- -----------------------------------------------

data CanTeachRel a = CanTeachRel deriving Typeable

instance Functor CanTeachRel where fmap _ = const CanTeachRel
type instance RelationDetails CanTeachRel = NoDetails
instance InformationRelation CanTeachRel where
    relationName _ = "CanTeach"
    coerceRelation = coerce

instance BinaryRelation CanTeachRel where
    binRelValue _ a b =
     let v ds c = if classDiscipline c `member` ds then 1 else 0
     in case collectInf a of
        Just (CanTeach ds)  -> let
            r1  = case collectInf b of Just (SomeClass c)  -> Just (v ds c, NoDetails)
            r2  = case collectInf b of Just (Class c)      -> Just (v ds c, NoDetails)
            in  r1 <|> r2
        _                   -> Nothing

-- -----------------------------------------------

data NeedsDisciplineRel a = NeedsDisciplineRel

instance Functor NeedsDisciplineRel where fmap _ = const NeedsDisciplineRel
type instance RelationDetails NeedsDisciplineRel = NoDetails
instance InformationRelation NeedsDisciplineRel where  relationName _ = "Needs Discipline"
                                                       coerceRelation = coerce
instance BinaryRelation NeedsDisciplineRel where
    binRelValue _ a b = do  let v ds c = if classDiscipline c `member` ds then 1 else 0
                            Needs ds <- collectInf a
                            let r1  = case collectInf b of Just (SomeClass c)  -> Just (v ds c, NoDetails)
                                r2  = case collectInf b of Just (Class c)      -> Just (v ds c, NoDetails)
                            r1 <|> r2

-- -----------------------------------------------

data MeetsRequirementsRel a = MeetsRequirementsRel

instance Functor MeetsRequirementsRel where fmap _ = const MeetsRequirementsRel
type instance RelationDetails MeetsRequirementsRel = NoDetails
instance InformationRelation MeetsRequirementsRel where  relationName _ = "Meets Requirements"
                                                         coerceRelation = coerce
instance BinaryRelation MeetsRequirementsRel where
    binRelValue _ a b = do  let v reqs c =  if all (`member` reqs)
                                                $ disciplineRequirements $ classDiscipline c
                                          then 1 else 0
                            RoomProvides rs <- collectInf a
                            let r1  = case collectInf b of Just (SomeClass c)  -> Just (v rs c, NoDetails)
                                r2  = case collectInf b of Just (Class c)      -> Just (v rs c, NoDetails)
                            r1 <|> r2


-- -----------------------------------------------

data EnoughCapacityRel a = EnoughCapacityRel

instance Functor EnoughCapacityRel where fmap _ _ = EnoughCapacityRel
type instance RelationDetails EnoughCapacityRel = NoDetails
instance InformationRelation EnoughCapacityRel where  relationName _ = "Enough Capacity"
                                                      coerceRelation = coerce
instance BinaryRelation EnoughCapacityRel where
    binRelValue r a b = do
                            -- let v :: (AbstractClass c, Num n) => Int -> c -> n
                            --     v size c =  if getGroupSize r (classGroup c) > size
                            --                 then 0 else 1
                            RoomCapacity cap  <- collectInf a
                            GroupSize grSize  <- collectInf b
                            let r1  = case collectInf b of Just (SomeClass c)  -> Just (v cap c, NoDetails)
                                r2  = case collectInf b of Just (Class c)      -> Just (v cap c, NoDetails)
                            r1 <|> r2


-- -----------------------------------------------

instance (Num a) => Context (Capabilities Role.Group) a where
  contextName _       = "Group Capabilities"
  contextInformation  = return . fromNodes . (:[])
                      . Information . Needs
                      . Set.fromList . needsDisciplines
  contextRelations _  = return [RelBin NeedsDisciplineRel]
  contextThreshold _  = return 0

  type AssessmentDetails (Capabilities Role.Group) = NoDetails

  combineWholeRels    = Combine.wholeRelsProduct (const NoDetails)
  combineBinRels      = Combine.binRelsProduct (const NoDetails)
  combineRels         = Combine.relsProduct (\_ _ -> NoDetails)

  noAssessmentDetails _ = NoDetails


instance (Num a) => Context (Capabilities Role.Professor) a where
  contextName _       = "Professor Capabilities"
  contextInformation  = return . fromNodes . (:[])
                      . Information . CanTeach
                      . Set.fromList . canTeachFullTime
  contextRelations _  = return [RelBin CanTeachRel]
  contextThreshold _  = return 0

  type AssessmentDetails (Capabilities Role.Professor) = NoDetails

  combineWholeRels    = Combine.wholeRelsProduct (const NoDetails)
  combineBinRels      = Combine.binRelsProduct (const NoDetails)
  combineRels         = Combine.relsProduct (\_ _ -> NoDetails)

  noAssessmentDetails _ = NoDetails



instance (Num a) => Context (Capabilities Role.Classroom) a where
  contextName _         = "Classroom Capabilities"
  contextInformation c  = return $ fromNodes [
                        Information . RoomProvides . Set.fromList
                            $ classroomCapabilities c,
                        Information . RoomCapacity
                            $ classroomCapacity c
                        ]

--    contextRelations _  = return [ RelBin MeetsRequirementsRel
--                                 , RelBin EnoughCapacityRel ]
--  contextThreshold _  = return 0

--  type AssessmentDetails (Capabilities Role.Professor) = NoDetails

--  combineWholeRels    = Combine.wholeRelsProduct (const NoDetails)
--  combineBinRels      = Combine.binRelsProduct (const NoDetails)
--  combineRels         = Combine.relsProduct (\_ _ -> NoDetails)

--  noAssessmentDetails _ = NoDetails


\end{code}
