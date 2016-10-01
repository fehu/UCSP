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

  import AUCSP.NegotiationRoles     as Role

  import Data.Typeable
  import Data.List (nub)
  import Data.Map (filterWithKey)

  import qualified Data.Map as Map
  import qualified Data.Set as Set

  import Control.Concurrent.STM
  import Control.Monad

\end{code}
%endif


%if standalone
\begin{document}
%endif


The initial negotiation environment is represented by the agents and their
internal knowledge. All the (initial) internal knowledge is found in the contexts.

\begin{code}



\end{code}

\subsubsection {Default \emph{root} controller}

Creates and controls the lifetime of \emph{agents managers}.

> defaultRootControllerDescriptor :: (EmptyResult res) => IO (RootControllerDescriptor res)
> defaultRootControllerDescriptor = do
>    cnt <- newTVarIO 0
>    return RootControllerDescriptor{
>        rootControllerIdPrefix   = "root",
>        rootControllerCount      = cnt,
>        rootNoResult             = emptyResult,
>        rootControllerDebug      = False,
>        rootControllerBehaviour  = AgentBehavior {

It performs no active action, only listens for the underlying controllers and executes orders.

>           agentAct = AgentNoAct,

Supports following orders:

>           handleMessages = AgentHandleMessages {
>               handleMessage = \i s -> selectMessageHandler [

\begin{itemize}

  \item \verb|StartMessage| and \verb|StopMessage|: resends the message to the underlying controllers;

>                   mbHandle $ \StartMessage  -> resendOrder s StartMessage,
>                   mbHandle $ \StopMessage   -> resendOrder s StopMessage,

  \item \red{TODO} \verb|CreateAgents|: orders a child controllers to create agents.

  \item \verb|ShareRefsBetweenRoles|: asks the children of some role for their agents references
        and orders the controllers of another role to share them;

>                   mbHandle $ \(ShareRefsBetweenRoles share) -> do
>                                   let  allFrom  = Set.fromList $ concatMap fst share
>                                   ctrls <- readTVarIO s
>                                   let fromRefs = filterWithKey  (\k _ -> k `Set.member` allFrom)
>                                                                 ctrls
>                                   refsByRole <- forM fromRefs (`askController` ReportAgentRefs)
>                                   sequence_ $ do  (from, to) <- share
>                                                   let refs = (=<<) (refsByRole Map.!)
>                                                   return $ forM (refs to)
>                                                                 (`send` ConnectWith (refs from))

\end{itemize}

It answers no messages, reporting progress via a 'NegotiationSysCtrl'.

>                   ],
>               respondMessage = \i s -> selectResponse []
>           }
>       }
>    }


> resendOrder s msg = readTVarIO s >>= mapM_ (`orderController` msg)



\subsubsection {Controller messages}


> data ShareRefsBetweenRoles = ShareRefsBetweenRoles [([AnyRole], [AnyRole])]
>   deriving (Typeable, Show)

> data ReportAgentRefs = ReportAgentRefs deriving (Typeable, Show)
> type instance ExpectedResponse ReportAgentRefs = [AgentRef]











\subsubsection {Default \emph{child} controller}

% defaultChildControllerDescriptor :: (EmptyResult res) => IO  -- TODO

A \emph{negotiation controller} (or \emph{agents manager}) creates negotiating agents,
and monitors their lifetime (including solution progeres), notifying the root about significant
changes.

> defaultControllerDescriptor waitTime = do
>     cnt <- newTVarIO 0
>     return $ controllerManagerDescriptor False AgentBehavior {

In it's asyncronous action it monitors the subjugated agents. The actions are defined
by `monitorStatus` and `monitorStatus'`.

>           agentAct = AgentActRepeat controllerAct waitTime,


managerBehaviour  = AgentBehavior (AgentActRepeat controllerAct waitTime)
                           $ AgentHandleMessages
                                (\_ m -> selectMessageHandler
                                    [  handleStart undefined, handleStop undefined
                                    ,  handleCreateAgents m ])
                                (\_ m -> selectResponse [ responseCreateAgents m ])

Supports following orders:

>           handleMessages = AgentHandleMessages {
>               handleMessage = \i s -> selectMessageHandler [

\begin{itemize}

  \item \verb||: ;

>

\end{itemize}

Answers following messages:

>                   ],
>               respondMessage = \i s -> selectResponse [

\begin{itemize}

 \item \verb||: ;

\end{itemize}


>                   ]
>           }
>       }









> instance EmptyResult SomeCandidate where emptyResult = NoCandidate


%if standalone
\end{document}
%endif


%%% local variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-build-standalone-flag: t
%%% eval: (haskell-indentation-mode)
%%% eval: (interactive-haskell-mode)
%%% End:

