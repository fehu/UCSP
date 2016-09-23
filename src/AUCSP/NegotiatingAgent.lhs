
%if False
\begin{code}
module AUCSP.NegotiatingAgent (

 Decider(..), DeciderUCSP(..), DecisionUCSP(..)

, negotiatingAgentDescriptor
, NextId(nextId)
, IDGenerators(..), IDGenerator(..)

, splitAndPropagateThroughContexts
, propagateThroughContexts

, AgentStates(..)
, ContextConstraints
, internalContexts

, negotiationAgentHandleMessages
, negotiatingAgentBehavior

) where

import Agent.Abstract
import Agent.Ask
import AUCSP.Classes
import AUCSP.Coherence
import AUCSP.Contexts
import qualified AUCSP.NegotiationRoles as Role

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
\item Accept some proposal as the chosen and ask the counterparts for confirmation.
\end{enumerate}

\begin{code}

data DeciderUCSP a = DeciderUCSP  {  newProposal   :: [Candidate a] -> IO Class
                                  ,  commonGoal    :: Candidate a -> a
                                  }

data DecisionUCSP a  =  AggregateProposal Class
                     |  AcceptCandidate (Candidate a)

instance (Ord a) => Decider DeciderUCSP a where
    type Decision DeciderUCSP a = IO (DecisionUCSP a)

    decide  d@DeciderUCSP{newProposal=newProp, commonGoal=goal}
            candidates =

\end{code}

A new proposal is added if no candidate was accepted, with an
exception for the candidates that are believed to to achieve much better
the \emph{common goal}. Otherwise the successeful proposal with the best
common goal completion is set as the \emph{accepted candidate}.

\begin{code}

           let  bestByGoal = sortWith (Down . snd) $ zipmapr goal candidates
                (successes, failures) = (candidateSuccess . fst) `span` bestByGoal
           in if null successes  then AggregateProposal <$> newProp (fst <$> bestByGoal)
                                 else return  . AcceptCandidate . fst . head $ successes

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

    decider   :: s -> DeciderUCSP a
    newStates :: DeciderUCSP a -> IO s

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

\red{TODO: AcceptCandidate}

\subsubsection{Messages handling}

Messages declarations.

\begin{code}

data NewClassAdded = NewClassAdded Class deriving (Typeable, Show)

-- -----------------------------------------------

data AcceptCandidateReq =  forall a. (Typeable a, Show a, Num a, Ord a) =>
                           AcceptCandidateReq (Candidate a) deriving Typeable
instance Show AcceptCandidateReq where
    show (AcceptCandidateReq c) = "AcceptCandidateReq(" ++ show c ++ ")"

data AcceptCandidateResp = WillAccept
                         | WontAccept
                   deriving (Typeable, Show, Eq)

type instance ExpectedResponse AcceptCandidateReq  = AwaitingResponse AcceptCandidateResp
type instance ExpectedResponse AcceptCandidateResp = ConfirmOrCancel

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

Agent's opinion about a class is the \emph{internal} (without considering the
\emph{external} context) coherence of the \red{one-class candidate (NO!)}.

\begin{code}

    mbResp $ \(OpinionAbout class') ->
        do let c = newCandidate [Information class']
           [c'] <- propagateThroughContexts [c] $ internalContexts state
           return . MyOpinion $ candidateSuccessCoherence c'

\end{code}

\red{TODO: AcceptCandidate}

> ] }


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Behavior constructor.

\begin{code}

type ContextConstraints s a =  ( Fractional a, Typeable a, Ord a, Show a
                               , AgentStates s a
                               , Context (Capabilities (ContextsRole s)) a
                               , Typeable (ContextsRole s))

internalContexts  :: ContextConstraints s a => s -> [SomeContext a]
internalContexts s = ($ s) <$> [  SomeContext . capabilitiesContext
                               ,  SomeContext . obligationsContext
                               ,  SomeContext . preferencesContext
                               ]

negotiatingAgentBehavior  :: (ContextConstraints s a)
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

AgentDescriptor

\begin{code}

data IDGenerator = IDGenerator {  idPrefix :: String
                               ,  lastId   :: IORef Integer
                               }
nextId_ gen = do  id <- fmap (+1) . readIORef $ lastId gen
                  writeIORef (lastId gen) id
                  return $ idPrefix gen ++ show id

data IDGenerators = IDGenerators{
    groupIdGen  :: IDGenerator,
    profIdGen   :: IDGenerator,
    roomIdGen   :: IDGenerator
    }

class (Contexts c a, ContextsRole c ~ r) => NextId c r a where
    nextId :: IDGenerators -> c -> IO String

instance (Contexts c a, ContextsRole c ~ Role.Group) => NextId c Role.Group a where
    nextId = const . nextId_ . groupIdGen
instance (Contexts c a, ContextsRole c ~ Role.Professor) => NextId c Role.Professor a where
    nextId = const . nextId_ . profIdGen
instance (Contexts c a, ContextsRole c ~ Role.Classroom) => NextId c Role.Classroom a where
    nextId = const . nextId_ . roomIdGen


negotiatingAgentDescriptor  :: (ContextConstraints s a, NextId s (ContextsRole s) a)
                            => IDGenerators -> DeciderUCSP a -> AgentDescriptor s
negotiatingAgentDescriptor gens decider = AgentDescriptor{
    agentBehaviour  = negotiatingAgentBehavior decider,
    newAgentStates  = newStates decider,
    nextAgentId     = fmap AgentId . nextId gens
    }


\end{code}


