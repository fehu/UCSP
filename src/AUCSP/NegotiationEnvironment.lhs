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

  FixedNegotiationEnvironment(..)
, createFixedEnvironment

, NegotiationStatus(..), newNegotiationStatus

) where

  import Agent.Controller
  import AUCSP.Context

  import Data.Typeable
  import Control.Concurrent.STM

\end{code}
%endif


%if standalone
\begin{document}
%endif


The initial negotiation environment is represented by the agents and their
internal knowledge.








\begin{code}

  data FixedNegotiationEnvironment = FixedNegotiationEnvironment {
    fixedEnvStatus          :: NegotiationStatus,
    fixedEnvRootController  :: SomeParentController
    }

  data NegotiationStatus = NegotiationStatus{
    negotiationBegan   :: TMVar Millis,
    negotiationEnded   :: TMVar Millis,
    negotiationResult  :: TMVar [SomeCandidate]
    }

  newNegotiationStatus =
    atomically $ do  began   <- newEmptyTMVar
                     ended   <- newEmptyTMVar
                     result  <- newEmptyTMVar
                     return $ NegotiationStatus began ended result


  createFixedEnvironment  :: ControllerSystemDescriptor
                          -> IO FixedNegotiationEnvironment
  createFixedEnvironment (ControllerSystemDescriptor rd ds) =
    do ctrl    <- newRootController rd ds
       status  <- newNegotiationStatus
       return  . FixedNegotiationEnvironment status
               $ SomeParentController ctrl


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

