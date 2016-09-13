
%if False
\begin{code}

module AUCSP.Context.Capabilities(

  Capabilities(GroupCapabilities, FullTimeProfCapabilities)

, CanTeachRel(..), NeedsDisciplineRel(..)

) where

import AUCSP.Classes
import AUCSP.NegotiationRoles
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

data instance Capabilities GroupRole a = GroupCapabilities {
  needsDisciplines :: [Discipline]
  }

data instance Capabilities ProfessorRole a = FullTimeProfCapabilities {
  canTeachFullTime :: [Discipline]
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

instance (Num a) => Context (Capabilities GroupRole) a where
  contextName _       = "Capabilities"
  contextInformation  = return . fromNodes . (:[])
                      . Information . Needs
                      . Set.fromList . needsDisciplines
  contextRelations _  = return [RelBin NeedsDisciplineRel]
  contextThreshold _  = return 0

  type AssessmentDetails (Capabilities GroupRole) = NoDetails

  combineWholeRels    = Combine.wholeRelsProduct (const NoDetails)
  combineBinRels      = Combine.binRelsProduct (const NoDetails)
  combineRels         = Combine.relsProduct (\_ _ -> NoDetails)

  noAssessmentDetails _ = NoDetails


instance (Num a) => Context (Capabilities ProfessorRole) a where
  contextName _       = "Capabilities"
  contextInformation  = return . fromNodes . (:[])
                      . Information . CanTeach
                      . Set.fromList . canTeachFullTime
  contextRelations _  = return [RelBin CanTeachRel]
  contextThreshold _  = return 0

  type AssessmentDetails (Capabilities ProfessorRole) = NoDetails

  combineWholeRels    = Combine.wholeRelsProduct (const NoDetails)
  combineBinRels      = Combine.binRelsProduct (const NoDetails)
  combineRels         = Combine.relsProduct (\_ _ -> NoDetails)

  noAssessmentDetails _ = NoDetails

\end{code}

