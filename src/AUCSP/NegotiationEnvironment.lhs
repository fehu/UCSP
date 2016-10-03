%if standalone
\documentclass{article}

%include src/Document/format.fmt
%include polycode.fmt
%include forall.fmt

\usepackage[english]{babel}
\usepackage[inline, shortlabels]{enumitem}

\usepackage{showframe}

\begin{document}
%endif

%if False
\begin{code}
module AUCSP.NegotiationEnvironment(

  newDefaultNegotiation
, defaultControllerDescriptor
, defaultRootControllerDescriptor
, RoleAgentsDescriptor(..)

-- , describeNegotiation, describeAgents, newAgent

, module Export
, module Role

)
where

  import Agent                      as Export
  import Agent.Controller           as Export
  import AUCSP.NegotiationStates    as Export
  import AUCSP.NegotiatingAgent     as Export
  import AUCSP.Context              as Export

  import AUCSP.NegotiationRoles     as Role

  import Data.Typeable
  import Data.List (nub)
  import Data.Maybe (fromJust)
  import Data.Map (filterWithKey)

  import qualified Data.Map as Map
  import qualified Data.Set as Set

  import Control.Concurrent.STM
  import Control.Monad

\end{code}
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
>                                   ctrls <- readTVarIO s
>                                   let  allFrom  = concatMap fst share
>                                        fromRefs = ctrlRefsForRoles allFrom ctrls
>                                   refsByRole <- forM fromRefs (`askController` ReportAgentRefs)
>                                   sequence_ $ do  (from, to) <- share
>                                                   let  refs = (=<<) (refsByRole Map.!)
>                                                        msg = ConnectWith (refs from)
>                                                   return $ forM (ctrlRefsForRoles to ctrls)
>                                                                 (`orderController` msg)

\end{itemize}

It answers no messages, reporting progress via a 'NegotiationSysCtrl'.

>                   ],
>               respondMessage = \i s -> selectResponse []
>           }
>       }
>    }


> resendOrder s msg = readTVarIO s >>= mapM_ (`orderController` msg)
> ctrlRefsForRoles roles = let rset = Set.fromList roles
>                          in filterWithKey  (\k _ -> k `Set.member` rset)


\subsubsection {Controller messages}


> data ShareRefsBetweenRoles = ShareRefsBetweenRoles [([AnyRole], [AnyRole])]
>   deriving (Typeable, Show)

> data ReportAgentRefs = ReportAgentRefs deriving (Typeable, Show)
> type instance ExpectedResponse ReportAgentRefs = [AgentRef]











\subsubsection {Default \emph{child} controller}



A \emph{negotiation controller} (or \emph{agents manager}) creates negotiating agents,
and monitors their lifetime (including solution progeres), notifying the root about significant
changes.

 defaultControllerDescriptor  :: Maybe Millis
                              -> IO (ManagerAgentDescriptor (AgentStatus' SomeCandidate))

> defaultControllerDescriptor

  :: (Agent.Manager.AgentsManager s s1,
      Agent.Controller.ChildController s,
      Agent.Controller.Controller s) =>

>  :: -- (Show (NegotiationResult s)) =>
>   Maybe Millis -> IO (ManagerAgentDescriptor (CManagerState SomeCandidate))

> defaultControllerDescriptor waitTime = controllerManagerDescriptor False AgentBehavior {

In it's asyncronous action it monitors the subjugated agents. The actions are defined
by `monitorStatus` and `monitorStatus'`.

>           agentAct = AgentActRepeat controllerAct waitTime,

Supports following orders:

>           handleMessages = AgentHandleMessages {
>               handleMessage = \_ s -> selectMessageHandler [

\begin{itemize}

  \item \verb|StartMessage| and \verb|StopMessage|: resends the message to all the agents;

>                   mbHandle $ \StartMessage  -> foreachAgent_ s (`send` StartMessage),
>                   mbHandle $ \StopMessage   -> foreachAgent_ s (`send` StopMessage),

  \item \verb|ConnectWith| message is also resent;

>                   mbHandle $ \msg@(ConnectWith _) -> foreachAgent_ s (`send` msg),

  \item \verb|CreateAgents|: creates and registers agents.

>                   handleCreateAgents s

\end{itemize}

Answers following messages:

>                   ],
>               respondMessage = \_ s -> selectResponse [

\begin{itemize}

 \item \verb|ReportAgentRefs|: returns currently registered agents;

>                   mbResp $ \ReportAgentRefs -> map fullRef2Ref <$> listAgents s,

 \item \verb|CreateAgents|: creates and registers agents; returns agents' references and shared states.

>                   responseCreateAgents s

\end{itemize}

>                   ]
>           }
>       }


\subsubsection{Negotiation Environment}


> type CreateNegotiationAgent r a = CreateAgent  (States r a)
>                                                SomeCandidate
>                                                (AgentRunOfRole r)
>                                                (AgentStatus' SomeCandidate)

> data RoleAgentsDescriptor r a = RoleAgentsDescriptor {
>       agentsRole          :: r,
>       ctrlWaitTime        :: Maybe Millis,
>       agentsDescriptors   :: [CreateNegotiationAgent r a] }


> newDefaultNegotiation roleDescrs = do
>       root  <- defaultRootControllerDescriptor
>       cads  <- sequence . flip map roleDescrs
>               $ \rd -> do
>                   md  <- defaultControllerDescriptor $ ctrlWaitTime rd
>                   return $ ControlledAgentsDescriptor md
>                          (agentsRole rd) (agentsDescriptors rd)
>       return  $ createControllerSystem
>               $ ControllerSystemDescriptor root cads



 type States' = forall r a . (Typeable r, Typeable a) => States r a

 newAgent :: (Typeable r) =>
             DeciderUCSP a -> (DeciderUCSP a -> IO (States r a)) ->
             Maybe Millis -> Bool -> IDGenerators -> CreateNegotiationAgent r a

> newAgent decider newStates waitTime debug idGens = CreateAgent descr extractStatusState
>    where  descr = negotiatingAgentDescriptor idGens decider waitTime newStates debug

>           extractStatusState :: AgentRunOfRole r -> AgentStatus' SomeCandidate
>           extractStatusState = undefined

           extractStatusState ag = fromJust $ getAgentState ag

           f :: (Typeable r) => AgentRunOfRole r -> AgentStatus' SomeCandidate


           f q = case findAgentState statusState q of Just s  -> s :: AgentStatus' SomeCandidate

 extractStatusState :: AgentRunOfRole r -> Maybe (AgentStatus' SomeCandidate)
 extractStatusState = undefined

 extractStatusState :: SomeStates -> AgentStatus' SomeCandidate
 extractStatusState (SomeStates s) = case cast s of Just States{statusState} -> statusState

       where f = extractSomeState f


 describeAgents  :: (Fractional a, Ord a, Show a, Typeable a, Typeable r ) =>
                    DeciderUCSP a -> Maybe Millis
                 -> Bool -> IDGenerators -> [DeciderUCSP a -> IO (States r a)]
                 -> [CreateNegotiationAgent r a]
 describeAgents decider waitTime debug idGens = map $ \ns -> newAgent decider ns waitTime debug idGens

 describeNegotiation :: Bool -> []

> describeNegotiation debug = map (($ newIDGenerators) . ($ debug))

 instance StatesAccess SomeStates (Maybe (AgentStatus' SomeCandidate))
   where getState

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

