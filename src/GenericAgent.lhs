\documentclass{article}

%include format.fmt
%include polycode.fmt
%include forall.fmt

%format ExpectedResponse1 = "ExpectedResponse_1"

\usepackage[english]{babel}
\usepackage[inline, shortlabels]{enumitem}

\usepackage{showframe}

%if False
\begin{code}

module GenericAgent where

  import Data.Typeable

  import Control.Concurrent
  import Control.Concurrent.STM
  import Control.Monad

\end{code}
%endif

\begin{document}

\subsubsection{Behavior definition}

Agent's behavior is defined by its \emph{action loop} and incoming
\emph{messages handling}.

\begin{code}

  data AgentBehavior states = AgentBehavior {
    act :: forall i . (AgentInnerInterface i) => i -> states -> IO (),
    handleMessages :: AgentHandleMessages states
    }

\end{code}

Messages can be just \emph{sent} to any agent or a specific \emph{response} may
be \emph{asked}.
\begin{code}

  class (Typeable ref, Ord ref) => AgentComm ref where
    agentId  :: ref -> AgentId
    send     :: (Message msg)     => ref -> msg    -> IO ()
    ask      :: (Message msg)     => ref -> msg    -> IO (ExpectedResponse msg)
    askT     :: (MessageT msg t)  => ref -> msg t  -> IO (ExpectedResponse1 msg t)

\end{code}

These messages are handled by the corresponding agent's functions.

> data AgentHandleMessages states = AgentHandleMessages {

\begin{itemize}
  \item react to sent messages (sent with \verb|send|):

>   handleMessage :: forall msg i .  ( Message msg
>                                    , AgentInnerInterface i) =>
>                           i -> states -> msg -> IO (),

  \item respond un-typed messages (responding to \verb|ask|):

>   respondMessage :: forall msg resp i .  ( Message msg
>                                          , ExpectedResponse msg ~ resp
>                                          , AgentInnerInterface i) =>
>                            i -> states -> msg -> IO resp,

  \item respond typed messages (responding to \verb|askT|):


>   respondTypedMessage :: forall msg resp t i .  ( MessageT msg t
>                                                 , ExpectedResponse1 msg t ~ resp t
>                                                 , AgentInnerInterface i) =>
>                                 i -> states -> msg t -> IO (resp t)
>   }

\end{itemize}

The expected response type should be defined for every message that is
intended to get responses.

> type family ExpectedResponse   (msg :: *)         :: *
> type family ExpectedResponse1  (msg :: * -> *)    :: * -> *


Restriction for messages is having instances of \verb|Typeable| and \verb|Show|.

> type Message msg     = (Typeable msg, Show msg)
> type MessageT msg t  = (Typeable t, Typeable msg, Show (msg t))


> data StartMessage  = StartMessage  deriving (Typeable, Show) -- Starts agent's act thread
> data StopMessage   = StopMessage   deriving (Typeable, Show) -- Terminates agent


Agent interface is used to reference agent-self within behavior definitions.

> class AgentInnerInterface i where  selfRef   :: i -> AgentRef
>                                    selfStop  :: i -> IO ()


\subsubsection{Role-depending behavior}
The expected response may depend on agent's \emph{role}.

\begin{code}

  class (AgentComm ref) => AgentCommRole ref where
    type AgentRole ref :: *

    askR  :: (Message msg)     => ref
                               -> msg
                               -> IO (ExpectedResponseForRole (AgentRole ref) msg)
    askRT :: (MessageT msg t)  => ref
                               -> msg t
                               -> IO (ExpectedResponseForRole1 (AgentRole ref) msg t)

  type family ExpectedResponseForRole   r (msg :: *)         :: *
  type family ExpectedResponseForRole1  r (msg :: * -> *)    :: * -> *


  -- System role.
  data System = System

  -- Generic role.
  data Generic = Generic

\end{code}

\subsubsection{Referencing agents}

Agents are identified (also compared and searched) by its \verb|AgentId|,
that must contain a \emph{\textbf{unique}} string, for example an UUID.

> data AgentId = AgentId String deriving (Show, Eq, Ord)


Normal agent reference is a container for types of class \verb|AgentComm|.

> data AgentRef = forall ref . (AgentComm ref) => AgentRef ref

A reference itself provides \verb|AgentComm| interface for the underlying agent.

\begin{code}

  instance AgentComm AgentRef where
    agentId  (AgentRef ref)  = agentId ref
    send     (AgentRef ref)  = send ref
    ask      (AgentRef ref)  = ask ref
    askT     (AgentRef ref)  = askT ref

\end{code}

It is used the referenced agent's id for establishing \verb|Eq| and \verb|Ord|
relations over it.

> instance Eq AgentRef  where AgentRef a == AgentRef b        = agentId a == agentId b
> instance Ord AgentRef where AgentRef a `compare` AgentRef b = agentId a `compare` agentId b



\subsubsection{Agent control}
Agents should support \emph{priority messages}, that are processed before any
normal message.

\begin{code}
  class (AgentComm ref) => AgentCommPriority ref where
    sendPriority  :: (Message msg)     => ref -> msg    -> IO ()
    askPriority   :: (Message msg)     => ref -> msg    -> IO (ExpectedResponse msg)
    askTPriority  :: (MessageT msg t)  => ref -> msg t  -> IO (ExpectedResponse1 msg t)

\end{code}

A \emph{control interface} should be based on the priority messages.

\begin{code}

  class (AgentCommPriority ag) => AgentControl ag where
    startAgent   :: ag -> IO ()
    stopAgent    :: ag -> IO ()
    stopAgentNow :: ag -> IO ()


\end{code}


\subsubsection{Agent extended referencing}
A \verb|AgentFullRef| is used for agent control and status monitoring.
It contains some instance of \verb|AgentControl| and the information
about agent's threads.

> data AgentFullRef =  forall ref . (AgentControl ref) =>
>                      AgentFullRef ref AgentThreads

Each agent is expected to be composed of two execution threads:
\emph{message handling} and \emph{actions}.

> data AgentThreads = AgentThreads  {  _actThread      :: AgentThread
>                                   ,  _messageThread  :: AgentThread
>                                   }
>

> fromFullRef (AgentFullRef ref _) = AgentRef ref
> extractThreads (AgentFullRef _ (AgentThreads act msg)) = (act, msg)

The information about agent's thread permits checking on its status,
waiting for it to finish or killing it, using the provided \verb|ThreadId|.

> data AgentThread = AgentThread {  _threadId        :: ThreadId
>                                ,  _threadFinished  :: IO Bool
>                                ,  _waitThread      :: IO ()
>                                }

> forceStopAgent :: AgentFullRef -> IO ()
> forceStopAgent fref = do  _killThread act
>                           _killThread msg
>     where  (act, msg)   = extractThreads fref
>            _killThread  = killThread . _threadId

> waitAgent :: AgentFullRef -> IO ()
> waitAgent fref = do _waitThread act
>                     _waitThread msg
>     where  (act, msg)   = extractThreads fref


Just like a normal reference, the full one is compared and tested by
the \verb|AgentId|.

> instance Eq AgentFullRef where
>   AgentFullRef a _ == AgentFullRef b _         =  agentId a == agentId b
> instance Ord AgentFullRef where
>   AgentFullRef a _ `compare` AgentFullRef b _  = agentId a `compare` agentId b

It also provides instances of \verb|AgentComm|, \verb|AgentCommPriority| and
\verb|AgentControl|.

\begin{code}

  instance AgentComm AgentFullRef where
    agentId  (AgentFullRef ref _)  = agentId ref
    send     (AgentFullRef ref _)  = send ref
    ask      (AgentFullRef ref _)  = ask ref
    askT     (AgentFullRef ref _)  = askT ref

  instance AgentCommPriority AgentFullRef where
    sendPriority  (AgentFullRef ref _) = sendPriority ref
    askPriority   (AgentFullRef ref _) = askPriority  ref
    askTPriority  (AgentFullRef ref _) = askTPriority ref

  instance AgentControl AgentFullRef where
    startAgent   (AgentFullRef ref _) = startAgent ref
    stopAgent    (AgentFullRef ref _) = stopAgent ref
    stopAgentNow (AgentFullRef ref _) = stopAgentNow ref

\end{code}


\subsubsection{Agent Creation}

Generic creation is defined in for types $\mathrm{from}$ and $\mathrm{ag}$.

> class (AgentControl ag) => AgentCreate from ag where
>   createAgent :: from -> IO (ag, AgentFullRef)

A simple \emph{agent descriptor} that can be used for agent creation.

\begin{code}

  data AgentDescriptor states = AgentDescriptor{
      agentBehaviour  :: AgentBehavior states,
      newAgentStates  :: IO states,
      nextAgentId     :: IO AgentId
    }

\end{code}


\subsubsection{Agent Management}
A manager registers/unregisters agent references and provides
agent-related operations over them.

\begin{code}
  class AgentsManager m where
    newAgentsManager :: IO m
    listAgents       :: m -> IO [AgentFullRef]
    registerAgent    :: m -> AgentFullRef -> IO ()
    unregisterAgent  :: m -> AgentFullRef -> IO ()

    mapAgents        :: (AgentFullRef -> IO a)   -> m -> IO [a]
    mapAgents_       :: (AgentFullRef -> IO ())  -> m -> IO ()

    foreachAgent     :: m -> (AgentFullRef -> IO a)   -> IO [a]
    foreachAgent_    :: m -> (AgentFullRef -> IO ())  -> IO ()

    foreachAgent   = flip mapAgents
    foreachAgent_  = flip mapAgents_

  class AgentsManagerOps m where
    agentsStopped      :: m -> IO Bool
    waitAllAgents      :: m -> IO ()

    sendEachAgent      :: (Message msg) => m -> msg -> IO ()
    orderEachAgent     :: (Message msg) => m -> msg -> IO ()

    createWithManager  :: (AgentCreate from ag) => m -> from -> IO (ag, AgentFullRef)



\end{code}

\end{document}


%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% eval: (haskell-indentation-mode)
%%% eval: (interactive-haskell-mode)
%%% eval: (load (concat (file-name-as-directory (projectile-project-root)) "publish-pdf.el"))
%%% End:

