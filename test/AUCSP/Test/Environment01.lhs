%if standalone
\documentclass{article}

%include src/Document/format.fmt
%include polycode.fmt
%include forall.fmt

\usepackage[english]{babel}
\usepackage[inline, shortlabels]{enumitem}

\usepackage{showframe}

\begin{document}
\section{Environment Tests}
%endif

%if False
\begin{code}
module Main where

import Agent
import Agent.Controller

import AUCSP.Contexts
import AUCSP.NegotiatingAgent
import AUCSP.NegotiationRoles (NegotiationRole)
import AUCSP.NegotiationStates
import AUCSP.Test.TestData01

import qualified AUCSP.NegotiationRoles as Role

import Data.Typeable (Typeable)

import Control.Monad


\end{code}
%endif

\subsection{ Environment Test I }

\subsubsection{ Data }
%include src/AUCSP/Test/TestData01.lhs

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\subsubsection{ Agents }

Creation:

\begin{code}

createAgent' :: ( Typeable r , NextId (States r a) r a
                , ContextConstraints (States r a) a )
             => DeciderUCSP a
             -> Maybe Millis
             -> (DeciderUCSP a -> IO (States r a))
             -> Bool
             -> IO (AgentRunOfRole NegotiationRole, AgentFullRef)
createAgent' d w ns = createAgent <=< descriptor d w ns

descriptor decider' wait newStates debug = do
    idGens <- newIDGenerators
    return $ negotiatingAgentDescriptor idGens decider' wait newStates debug


groupAgent  :: AgentStatus SomeCandidate
            -> Maybe Millis
            -> ContextsHolder Role.Group Float
            -> Bool
            -> IO (AgentRunOfRole NegotiationRole, AgentFullRef)

groupAgent d w      = createAgent' groupDecider w      . groupStates d
professorAgent d w  = createAgent' professorDecider w  . professorStates d
classroomAgent d w  = createAgent' classroomDecider w  . professorStates d


fixedTest = ControllerSystemDescriptor
                undefined
                []

\end{code}


descriptor :: ( Typeable r, NextId (States r a) r a
              , ContextConstraints (States r a) a )
           => DeciderUCSP a
           -> (DeciderUCSP a -> IO (States r a))
           -> IO (AgentDescriptor (States r a) SomeCandidate)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Contexts:

%\begin{code}

%\end{code}




Deciders:

\begin{code}

decider' :: DeciderUCSP Float
decider' = DeciderUCSP {
  newProposal = undefined,
  commonGoal  = undefined
  }

groupDecider = decider'
professorDecider = decider'
classroomDecider = decider'

\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\begin{code}
main = undefined
\end{code}

%if standalone
\end{document}
%endif


%%% local variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-build-standalone-flag: t
%%% eval: (haskell-indentation-mode)
%%% eval: (interactive-haskell-mode)
%%% End:

