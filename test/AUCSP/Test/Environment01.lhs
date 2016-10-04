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

import AUCSP.NegotiationEnvironment
import AUCSP.Test.TestData01

-- import Data.Typeable (Typeable)

import Control.Monad


\end{code}
%endif

\subsection{ Environment Test I }

\subsubsection{ Data }
%include src/AUCSP/Test/TestData01.lhs

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Deciders:

\begin{code}

decider' :: DeciderUCSP Double
decider' = DeciderUCSP {
  newProposal = undefined,
  commonGoal  = undefined
  }

groupDecider = decider'
professorDecider = decider'
classroomDecider = decider'

\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Descriptors:

\begin{code}

describeGroups      = describeRole Group groupDecider groupStates [groupCtx]
describeProfessors  = describeRole  FullTimeProfessor professorDecider
                                    professorStates [profCtx1, profCtx2]
describeClassrooms  = describeRole  Classroom classroomDecider
                                    classroomStates [roomCtx1, roomCtx2]

\end{code}




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

