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
             -> (DeciderUCSP a -> IO (States r a))
             -> IO (AgentRunOfRole NegotiationRole, AgentFullRef)
createAgent' d = createAgent <=< descriptor d

descriptor :: ( Typeable r, NextId (States r a) r a
              , ContextConstraints (States r a) a )
           => DeciderUCSP a
           -> (DeciderUCSP a -> IO (States r a))
           -> IO (AgentDescriptor (States r a))
descriptor decider' newStates = do
    idGens <- newIDGenerators
    return $ negotiatingAgentDescriptor idGens decider' newStates


groupAgent  :: AgentStatus
            -> ContextsHolder Role.Group Float
            -> IO (AgentRunOfRole NegotiationRole, AgentFullRef)

groupAgent d      = createAgent' groupDecider . groupStates d
professorAgent d  = createAgent' professorDecider . professorStates d
classroomAgent d  = createAgent' classroomDecider . professorStates d


fixedTest = ControllerSystemDescriptor
                undefined
                []

\end{code}


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

