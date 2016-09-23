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

) where

  import Agent.Abstract
  import AUCSP.NegotiatingAgent
-- import qualified AUCSP.NegotiationRoles as Role
  import AUCSP.Context

  import Control.Exception
  import Control.Concurrent.STM

\end{code}
%endif


%if standalone
\begin{document}
%endif


The initial negotiation environment is represented by the agents and their
internal knowledge.

\begin{code}

  data AgentStatus a  =  Initialized
                      |  Negotiating
                      |  Waiting (Candidate a)
                      |  Locked
                      |  Terminated (SomeException)

  type Millis = Integer -- Milliseconds

  type AgentWithStatus a = (AgentFullRef, TVar (AgentStatus a))

  data NegotiationEnvironment a = NegotiationEnvironment{
    negotiationBegan  ::  TVar (Maybe Millis),
    negotiationEnded  ::  TVar (Maybe Millis)
    }

\end{code}

The negotiation is created and monitored by the \emph{controller(s)}, that
may be composed into hierarchical structure.

> class Controller c a | c -> a where

\begin{itemize}
\item Creates agents from descriptors, returning the corresponding references.

>  newAgents :: forall states . c -> [AgentDescriptor states] -> IO [AgentWithStatus a]

\item Guards all the created agents.

>  negotiatingAgents :: c -> IO [AgentWithStatus a]

\item Monitors agent's status.

>  type ControllerAction c :: *
>  monitorStatus   :: AgentWithStatus a -> STM (ControllerAction c)
>  monitorStatus'  :: c -> [ControllerAction c] -> IO ()

\item Notifies the parent controller.

>  type ControllerNotification c :: *
>  parentController  :: forall p . (Controller p a
>                                  , ControllerNotification c ~ ControllerNotification p)
>                    => c -> Maybe p
>  notifyParentCtrl  :: c -> ControllerNotification c -> IO ()

\end{itemize}


Controller implementation:

\begin{code}

  data ControllerImpl a = ControllerImpl {

  }

--  instance Controller ControllerImpl

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

