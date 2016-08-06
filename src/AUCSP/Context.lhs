
%if False
\begin{code}

module AUCSP.Context(

  Context(..), SomeContext(..)
, SplittingContext(..)

, AssessmentDetails(..)

, AssessedCandidate(..), Candidate(..)
, candidateSuccess

, assessWithin

, CBin(..), getCBin
, CWhole(..), getCWhole

, AnyFunc1

) where

import AUCSP.Coherence

import Data.Typeable
import Data.Either (partitionEithers)

import qualified Data.Map as Map

\end{code}
%endif


In order to use contexts for information \emph{coherence assessment},
the concepts of \emph{context-specific information graph} and
\emph{assessed information} are introduced.
The context-specific graph holds the information, already known/accepted by the
agent, and is relevant for the context in question.
The assessed one is \emph{assumed} during the evaluation process.

\begin{figure}[H]
  \centering
  \fbox{ \input{Document/tikz/ContextAssess} }
  \caption{Binary relations within an information graph. One can
           distinguish the relations between the assessed information pieces
           and the relations between assessed and the known ones.
          }
\end{figure}

To assess some information, it's propagated through the contexts, in the
\emph{specified order}, that stands for contexts priority. Each context
should have a \emph{coherence threshold} specified; after the assessed
information's coherence has been estimated, it's compared against the
threshold and either \texttt{Success} or \texttt{Failure} is returned,
along with the evaluated coherence value.
The information, that has successfully passed a context, is propagated
further; otherwise the failure is returned.

\begin{code}

class Context (c :: * -> *) a where
  contextName         :: c a -> String
  contextInformation  :: c a -> IO IGraph
  contextRelations    :: c a -> IO [IRelation a]
  contextThreshold    :: c a -> IO a

  combineBinRels      :: c a -> RelValsBetween a    -> Maybe (CBin a)
  combineWholeRels    :: c a -> RelValsWhole a      -> Maybe (CWhole a)
  combineRels         :: c a -> CBin a -> CWhole a  -> a


newtype CBin a    = CBin a
newtype CWhole a  = CWhole a

getCBin    (CBin a)    = a
getCWhole  (CWhole a)  = a

data AssessmentDetails a                -- TODO
    = AssessmentDetails deriving Show

data SomeContext a = forall c . Context c a => SomeContext (c a)

instance Show (SomeContext a) where
    show (SomeContext c) = "Context " ++ show (contextName c)

-- -----------------------------------------------

type AnyFunc1 res = forall a . a -> res a

mapEither :: AnyFunc1 r -> Either a b -> Either (r a) (r b)
mapEither f (Left a)   = Left $ f a
mapEither f (Right a)  = Right $ f a

assessWithin' ::  (Context c a, Fractional a, Typeable a, Show a) =>
                  [Information]
              ->  c a
              ->  IO (Maybe a, AssessmentDetails a)

assessWithin' inf c = do
  contextInf   <- contextInformation c
  contextRels  <- contextRelations c

  let  assumed  = contextInf `graphJoin` inf
       relsIO   = sequence  $ (\r -> mapEither ((,) r) <$> r `relationOn` assumed)
                            <$> contextRels
  (bins, whole) <- partitionEithers <$> relsIO

  let  assessed = case (bins, whole) of
                    ([], [])  -> Nothing
                    (_, [])   -> getCBin    <$> c `combineBinRels`    Map.fromList bins
                    ([], _)   -> getCWhole  <$> c `combineWholeRels`  Map.fromList whole
                    _         -> do  rBin    <- c `combineBinRels`    Map.fromList bins
                                     rWhole  <- c `combineWholeRels`  Map.fromList whole
                                     return  $ combineRels c rBin rWhole
  return (assessed, undefined)

-- -----------------------------------------------

data AssessedCandidate a = AssessedCandidate {
       assessedAt       :: SomeContext a
    ,  assessedVal      :: Maybe a
    ,  assessedDelails  :: AssessmentDetails a
    }
    deriving Show

data Candidate a   =  Success  {  assessHistory  :: [AssessedCandidate a]
                               ,  candidate      :: [Information]
                               }
                   |  Failure  {  assessHistory  :: [AssessedCandidate a]
                               ,  candidate      :: [Information]
                               }
    deriving Show

candidateSuccess Success{}  = True
candidateSuccess _          = False

-- -----------------------------------------------

assessWithin ::  (Context c a, Fractional a, Ord a, Typeable a, Show a) =>
                 Candidate a -> c a -> IO (Candidate a)

assessWithin f@Failure{} _ = return f
assessWithin (Success hist c) cxt = do
  (mbA, details)  <- c `assessWithin'` cxt
  threshold       <- contextThreshold cxt
  let ac = AssessedCandidate (SomeContext cxt) mbA details

  return $  if mbA > Just threshold
            then  Success  (ac : hist) c
            else  Failure  (ac : hist) c

\end{code}


\medskip\noindent
Some contexts might also be capable of \emph{splitting}
information graphs into \emph{valid candidates} --
the sub-graphs, that are \emph{valid} at the context.
The candidates can be assessed by the rest of the contexts.

\begin{code}

class (Context c a) => SplittingContext c a where
  splitGraph :: c a -> IGraph -> IO [Candidate a]

\end{code}



%include Contexts/Combine.lhs
%include Contexts/InUnitInterval.lhs

%include Contexts/Capabilities.lhs
%include Contexts/Beliefs.lhs
%include Contexts/Obligations.lhs
%include Contexts/Preferences.lhs
%include Contexts/External.lhs


