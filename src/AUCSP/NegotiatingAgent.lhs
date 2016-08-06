
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

\begin{code}

class (Contexts s a) => AgentStates s a | s -> a
  where
--    knownProposals


\end{code}


% Decisions execution:
%\begin{code}
%-- execDecision (AggregateProposal c) =
%\end{code}


\subsubsection{Messages handling}

Messages declarations (apart from those defined previously).

\begin{code}

data NewClassAdded = NewClassAdded Class deriving (Typeable, Show)

-- -----------------------------------------------

data AskedToYield =  forall a. (Typeable a, Show a) =>
                     AskedToYield (Candidate a) deriving Typeable
instance Show AskedToYield where show (AskedToYield c) = show c

data YieldResponse = WillYield | WontYield deriving (Typeable, Show)

type instance ExpectedResponse AskedToYield = YieldResponse

-- -----------------------------------------------

-- data

\end{code}


\begin{code}
negotiationAgentHandleMessages :: (AgentStates s a, Typeable a, Fractional a) => AgentHandleMessages s
negotiationAgentHandleMessages = AgentHandleMessages
    {
\end{code}

Handle simple messages (without response).

\begin{code}

    handleMessage = \i state msg ->
        case cast msg of Just (NewClassAdded c) -> let knownRef  = knownProposals
                                                                 $ beliefsContext state
                                                   in modifyIORef' knownRef -- strict
                                                      (`graphJoin` [Information c])

\end{code}

Untyped messages responses.

\begin{code}

  , respondMessage = \i state msg ->
        case cast msg of Just (AskedToYield candidate) -> undefined -- TODO

\end{code}

Typed messages responses.

\begin{code}

  , respondTypedMessage = \i state msg ->
        case cast' msg of Just (OpinionAbout c) ->  undefined -- TODO

--        case gcast msg of Just (OpinionAbout (c,a)) ->  let op = MyOpinion . inUnitInterval $ a
--                                                            Just r = cast op
--                                                        in return r

\end{code}

\begin{code}
    }


cast' :: (Typeable (a x), Typeable (b x), Typeable x) => a x -> Maybe (b x)
cast' = cast

\end{code}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Behavior constructor.

\begin{code}

negotiatingAgentBehavior  ::  ( Fractional a, Typeable a, Ord a, Show a
                              , AgentStates s a
                              , Decider d a)
                          => d a -> (Decision d a -> IO ()) -> AgentBehavior s
negotiatingAgentBehavior d applyD = AgentBehavior
  { act = \i s -> let  c0  = beliefsContext s
                       cs  = ($ s) <$>  [  -- SomeContext . capabilitiesContext
                                           SomeContext . obligationsContext
                                        ,  SomeContext . preferencesContext
                                        ,  SomeContext . externalContext
                                        ]
                  in applyD  <$>  decide d
                             =<<  splitAndPropagateThroughContexts c0 cs
                             =<<  contextInformation c0

  , handleMessages = negotiationAgentHandleMessages
  }


\end{code}



\begin{code}

\end{code}

