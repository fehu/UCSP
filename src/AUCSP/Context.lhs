
%if False
\begin{code}

module AUCSP.Context(

  Context(..), SomeContext(..)
, SplittingContext(..)

, AssessmentDetails(..)
, ContextDetails (..)
, NoDetails(..)


, assessedVal', extractSomeCxtDetails

, CandidateAssessment(..)
, SomeCandidateAssessment(..)
, Candidate(..), newCandidate
, mbCandidateCoherence, candidateCoherence
, candidateSuccess, candidateSuccessCoherence

, assessWithin

, CBin(..), getCBin
, CWhole(..), getCWhole

, AnyFunc1

) where

import AUCSP.Coherence

import Data.Typeable
import Data.Either (partitionEithers)
import Data.Maybe (fromMaybe)

import qualified Data.Map as Map

import Control.Arrow ( (&&&), first, second )

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


-- -----------------------------------------------

type family AssessmentDetails (c :: a -> *)  :: a -> *

class (Context c a, AssessmentDetails c a ~ details a) =>
    ContextDetails  (c :: * -> *) (details :: * -> *) a
  where
  type PartialAssessmentDetails c details :: * -> *

  binRelsDetails    :: c a  -> RelValsBetween a
                            -> Maybe (CBin a, PartialAssessmentDetails c details a)

  wholeRelsDetails  :: c a  -> RelValsWhole a
                            -> Maybe (CWhole a, PartialAssessmentDetails c details a)

  relsDetails       :: c a  -> (CBin a, PartialAssessmentDetails c details a)
                            -> (CWhole a, PartialAssessmentDetails c details a)
                            -> (a, details a)

  completePartialDetails  :: c a  -> PartialAssessmentDetails c details a
                                  -> details a
  emptyAssessmentDetails  :: c a  -> details a


-- -----------------------------------------------


data NoDetails a = NoDetails

withNoDetails :: Maybe (c a) -> Maybe (c a, NoDetails a)
withNoDetails = fmap (flip (,) NoDetails)

instance (Context c a, AssessmentDetails c a ~ NoDetails a) => ContextDetails c NoDetails a
  where
    type PartialAssessmentDetails c NoDetails = NoDetails
    binRelsDetails c    = withNoDetails . combineBinRels c
    wholeRelsDetails c  = withNoDetails . combineWholeRels c
    relsDetails c (b,_) (w,_) = (combineRels c b w, NoDetails)
    completePartialDetails _ = id
    emptyAssessmentDetails _ = NoDetails

-- -----------------------------------------------

data SomeContext a = forall c d . (ContextDetails c d a, Typeable c) => SomeContext (c a)

instance Show (SomeContext a) where
    show (SomeContext c) = "Context " ++ show (contextName c)

-- -----------------------------------------------

type AnyFunc1 res = forall a . a -> res a

mapEither :: AnyFunc1 r -> Either a b -> Either (r a) (r b)
mapEither f (Left a)   = Left $ f a
mapEither f (Right a)  = Right $ f a

assessWithin' ::  ( ContextDetails c d a, Fractional a, Typeable a, Show a
                  , AssessmentDetails c a ~ d a) =>
                  [Information]
              ->  c a
              ->  IO (CandidateAssessment c a)

assessWithin' inf c = do
  contextInf   <- contextInformation c
  contextRels  <- contextRelations c

  let  assumed  = contextInf `graphJoin` inf
       relsIO   = sequence  $ (\r -> mapEither ((,) r) <$> r `relationOn` assumed)
                            <$> contextRels
  (bins, whole) <- partitionEithers <$> relsIO

  let  assessedWithDetails = case (bins, whole) of
                    ([], [])  -> Nothing
                    (_, [])   -> first getCBin .
                                 second (completePartialDetails c)
                                 <$> c `binRelsDetails` Map.fromList bins
                    ([], _)   ->
                                 first getCWhole .
                                 second (completePartialDetails c)
                                 <$> c `wholeRelsDetails`  Map.fromList whole
                    _         -> do  b  <- c `binRelsDetails`    Map.fromList bins
                                     w  <- c `wholeRelsDetails`  Map.fromList whole
                                     return $ relsDetails c b w

       assessed  = fst <$> assessedWithDetails
       details   = maybe (emptyAssessmentDetails c) snd assessedWithDetails
  return $ CandidateAssessment assessed c details

-- -----------------------------------------------

data CandidateAssessment c a = CandidateAssessment {
       assessedVal      :: Maybe a
    ,  assessedAt       :: c a
    ,  assessedDelails  :: AssessmentDetails c a
    }

data SomeCandidateAssessment a = forall c . (Context c a, Typeable c) =>
     SomeCandidateAssessment (CandidateAssessment c a)

assessedVal' (SomeCandidateAssessment c) = assessedVal c

extractSomeCxtDetails :: (Typeable c, Typeable a)  =>  SomeCandidateAssessment a
                                                   ->  Maybe (c a, AssessmentDetails c a)
extractSomeCxtDetails (SomeCandidateAssessment c) = (assessedAt &&& assessedDelails) <$> cast c

instance Show (CandidateAssessment c a)   where  -- TODO
instance Show (SomeCandidateAssessment a) where  -- TODO

data Candidate a   =  Success  {  assessHistory  :: [SomeCandidateAssessment a]
                               ,  candidate      :: [Information]
                               }
                   |  Failure  {  assessHistory  :: [SomeCandidateAssessment a]
                               ,  candidate      :: [Information]
                               }
    deriving Show

candidateSuccess Success{}  = True
candidateSuccess _          = False

mbCandidateCoherence :: Candidate a -> Maybe a
mbCandidateCoherence c = case assessHistory c of  []    -> Nothing
                                                  (a:_) -> assessedVal' a

candidateCoherence c = fromMaybe 0 $ mbCandidateCoherence c

candidateSuccessCoherence c | candidateSuccess c  = candidateCoherence c
candidateSuccessCoherence _                       = 0

newCandidate = Success []

-- -----------------------------------------------

assessWithin ::  ( ContextDetails c d a, Fractional a, Ord a, Typeable a, Show a, Typeable c
                 , AssessmentDetails c a ~ d a ) =>
                 Candidate a -> c a -> IO (Candidate a)

assessWithin f@Failure{} _ = return f
assessWithin (Success hist c) cxt = do
  assessed   <- c `assessWithin'` cxt
  threshold  <- contextThreshold cxt

  let assessed' = SomeCandidateAssessment assessed

  return $  if assessedVal assessed > Just threshold
            then  Success  (assessed' : hist) c
            else  Failure  (assessed' : hist) c

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


