%if standalone
\documentclass{article}

%include src/Document/format.fmt
%include polycode.fmt
%include forall.fmt

\usepackage[english]{babel}
\usepackage[inline, shortlabels]{enumitem}

\usepackage{showframe}

%\begin{document}
%endif

%if False
\begin{code}
module AUCSP.NegotiationEnvironment (

  module Export
,

) where

  import Agent                      as Export
  import Agent.Controller           as Export
  import AUCSP.NegotiationStates    as Export
  import AUCSP.NegotiatingAgent     as Export
  import AUCSP.Context              as Export

  import Data.Typeable
  import Control.Concurrent.STM

\end{code}
%endif


%if standalone
\begin{document}
%endif


The initial negotiation environment is represented by the agents and their
internal knowledge. All the (initial) internal knowledge is found in the contexts.

\begin{code}



\end{code}




\begin{code}

  instance EmptyResult SomeCandidate where emptyResult = NoCandidate


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

