
%if False
\begin{code}
module AUCSP.NegotiatingAgent (

) where

import GenericAgent
import AUCSP.Classes
import AUCSP.Coherence
import AUCSP.Contexts

import Data.Typeable (Typeable, cast, gcast)
import Data.List (span)
import Data.Function (on)
import Data.IORef

import Control.Monad

import GHC.Exts (sortWith, Down(..))

\end{code}
%endif


\subsubsection{Behavior}

The candidates are \emph{propagated} through the contexts where they get assessed.
The initial candidaes are created from the known proposals by \emph{beliefs} splitting context.

\begin{code}

splitAndPropagateThroughContexts ::  ( SplittingContext c0 a
                                     , Fractional a, Ord a, Show a, Typeable a) =>
    c0 a -> [SomeContext a] -> IGraph -> IO [Candidate a]

splitAndPropagateThroughContexts splitting ctxs g =
    do  candidates <- splitting `splitGraph` g
        propagateThroughContexts candidates ctxs



propagateThroughContexts ::  (Fractional a, Ord a, Show a, Typeable a) =>
                             [Candidate a]-> [SomeContext a] -> IO [Candidate a]

propagateThroughContexts cs [] = return cs
propagateThroughContexts cs (SomeContext ctx : t) =
    (`propagateThroughContexts` t) =<< mapM (`assessWithin` ctx) cs

\end{code}

The decision is made after. It's based on the received candidates.

\begin{code}

class Decider d a where  type Decision d a :: *
                         decide :: d a -> [Candidate a] -> Decision d a

\end{code}

There possible decisions are:
\begin{enumerate}
\item Aggregate a new proposal.
\item Ask counterpart(s) to \emph{yield}.
\item Accept some proposal as the chosen and ask the counterparts for confirmation.
\end{enumerate}

\begin{code}

data DeciderUCSP a = DeciderUCSP  {  newProposal   :: [Candidate a] -> IO Class
                                  ,  commonGoal    :: Candidate a -> a
                                  ,  commonBetter  :: a -> a -> Bool
                                  }

data DecisionUCSP a  =  AggregateProposal Class
                     |  AskToYield (Candidate a)
                     |  AcceptCandidate (Candidate a)

instance (Ord a) => Decider DeciderUCSP a where
    type Decision DeciderUCSP a = IO (DecisionUCSP a)

    decide  d@DeciderUCSP{commonGoal=goal, commonBetter=better}
            candidates =

\end{code}

A new proposal is added if no candidate was accepted, with an
exception for the candidates that are believed to to achieve much better
the \emph{common goal}. Otherwise the successeful proposal with the best
common goal completion is set as the \emph{accepted candidate}.

\begin{code}

           let  bestByGoal = sortWith (Down . snd) $ zipmapr goal candidates

                muchBetter  = better `on` snd
                isSuccess   = candidateSuccess . fst

                (successes, failures) = (candidateSuccess . fst) `span` bestByGoal
                mbAskYield = case bestByGoal of
                                    (theBest:sndBest:_) | isSuccess theBest             -> Nothing
                                                        | theBest `muchBetter` sndBest  -> Just theBest
                                                        | otherwise                     -> Nothing
                                    _ -> Nothing

           in case mbAskYield of  Just (askYield, _)   -> return $ AskToYield askYield
                                  _  | null successes  ->  AggregateProposal <$>
                                                           newProposal d  (fst <$> bestByGoal)
                                  _                    -> return  . AcceptCandidate . fst
                                                                  . head $ successes


zipmapl :: (a -> b) -> [a] -> [(b, a)]
zipmapl f = map $ \x -> (f x, x)

zipmapr :: (a -> b) -> [a] -> [(a, b)]
zipmapr f = map $ \x -> (x, f x)

\end{code}

The agents need some states to be guarded. They are abstracted by the following definition.

\red{?? Needed ??}

\begin{code}

class (Contexts s a, Num a) => AgentStates s a | s -> a
  where
    getKnownClasses     :: s -> IO IGraph
    modifyKnownClasses  :: s -> (IGraph -> IGraph) -> IO ()

    getBestCandidate  :: s -> IO (Candidate a)
    setBestCandidate  :: s -> Candidate a -> IO ()

    decider :: s -> DeciderUCSP a

    getKnownClasses = readIORef . knownProposals . beliefsContext
    -- modification is strict
    modifyKnownClasses s = modifyIORef' . knownProposals $ beliefsContext s



\end{code}


Decisions execution.
\begin{code}
execDecision :: (ContextConstraints s a) => DeciderUCSP a -> s -> DecisionUCSP a -> IO ()
\end{code}

A new class should be added by every agent, mentioned in the class.

\begin{code}
execDecision d s (AggregateProposal cl@(Class c)) =
    do modifyKnownClasses s (`graphJoin` [Information cl])
       forM_ (counterpartsOf s c) ((`send` NewClassAdded cl) . knownAgentRef)

\end{code}

An agent should ask each counterpart of every contradicting class to \emph{yield}.
If any rejects the demand, then those counterpart must cancel yeild procedure, while
the demanding agent must yield instead.

\begin{code}

execDecision d s (AskToYield c@(Failure {assessHistory=(h:_), candidate=cand} )) =
    do let CandidateAssessment {assessedAt=at, assessedDelails=details} = h
       case at
        of SomeContext ctx ->
            when  (contextName ctx /= "External")
                  (fail "Expected assessment at the \"External\" context")
       case collectInf' cand
        of Just (Class class') ->                                           -- TODO: class match
               do let ags = s `counterpartsOf` class'
                  resps <- forM ags
                           $ \ka -> ask  (knownAgentRef ka)
                                         (AskedToYield c)
                  if all (WillYield == ) resps
                    then markYieldedTo c
                    else forM_ ags
                       $ \ka -> send  (knownAgentRef ka)
                                      (CancelYield c)

-- TODO
markYieldedTo :: Candidate a -> IO ()
markYieldedTo c = undefined

\end{code}


\subsubsection{Messages handling}

Messages declarations (apart from those defined previously).

\begin{code}

data NewClassAdded = NewClassAdded Class deriving (Typeable, Show)

-- -----------------------------------------------

data AskedToYield =  forall a. (Typeable a, Show a, Num a, Ord a) =>
                     AskedToYield (Candidate a) deriving Typeable
instance Show AskedToYield where show (AskedToYield c) = show c

data YieldResponse = WillYield | WontYield deriving (Typeable, Show, Eq)

type instance ExpectedResponse AskedToYield = YieldResponse

data CancelYield = forall a. (Typeable a, Show a, Num a, Ord a) =>
                   CancelYield (Candidate a) deriving Typeable

instance Show CancelYield where show (CancelYield c) = "CancelYield(" ++ show c ++ ")"

-- -----------------------------------------------

\end{code}


\begin{code}
negotiationAgentHandleMessages :: (ContextConstraints s a) => AgentHandleMessages s
negotiationAgentHandleMessages = AgentHandleMessages {
\end{code}

Handle simple messages (without response).

\begin{code}

    handleMessage = \i state msg ->
        case cast msg of Just (NewClassAdded c) ->  modifyKnownClasses state
                                                    (`graphJoin` [Information c])

\end{code}

Respond messages.

> , respondMessage = \i state -> selectResponse [

Yield decision consists in comparing the coherence, achieved by another agent,
with the best coherence, achieved by the current one. Coherence superiority
must be significant.

\begin{code}

      mbResp $ \(AskedToYield candidate) ->
        do  best <- getBestCandidate state
            let Just d         = cast $ decider state
                Just myBestCoh = cast $ candidateSuccessCoherence best
                itBestCoh      = candidateCoherence candidate
            if commonBetter d myBestCoh itBestCoh  then do  yieldTo candidate
                                                            return WillYield
                                                   else return WontYield

\end{code}

Agent's opinion about a class is the \emph{internal} (without considering the
\emph{external} context) coherence of the one-class candidate.

\begin{code}

    , mbResp $ \(OpinionAbout class') ->
        do let c = newCandidate [Information class']
           [c'] <- propagateThroughContexts [c] $ internalContexts state
           return . MyOpinion $ candidateSuccessCoherence c'

\end{code}

> ] }


Classes, contradicting the better candidate (yielded to), should have their coherence decreased.
\red{??? TODO ???}

\begin{code}

yieldTo = undefined -- TODO

\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Behavior constructor.

\begin{code}

type ContextConstraints s a =  ( Fractional a, Typeable a, Ord a, Show a
                               , AgentStates s a
                               , Context (Capabilities (ContextsRole s)) a)

internalContexts  :: ContextConstraints s a => s -> [SomeContext a]
internalContexts s = ($ s) <$> [  SomeContext . capabilitiesContext
                               ,  SomeContext . obligationsContext
                               ,  SomeContext . preferencesContext
                               ]

negotiatingAgentBehavior  :: ( ContextConstraints s a)
                          => DeciderUCSP a -> AgentBehavior s
negotiatingAgentBehavior d = AgentBehavior
  { act = \i s -> let  c0  = beliefsContext s
                       cs  = internalContexts s ++ [SomeContext $ externalContext s]
                  in execDecision d s  =<<  decide d
                                       =<<  splitAndPropagateThroughContexts c0 cs
                                       =<<  contextInformation c0

  , handleMessages = negotiationAgentHandleMessages
  }

\end{code}



\begin{code}

\end{code}
