%if standalone
\documentclass{article}

%include format.fmt
%include polycode.fmt
%include forall.fmt

\usepackage[english]{babel}
\usepackage[inline, shortlabels]{enumitem}

\usepackage{showframe}

%endif

%if False
\begin{code}

module GenericAgent(

  AgentComm(..)
, ExpectedResponse, ExpectedResponse1
, AgentRef(..), AgentRef'(..)

, AgentCommRole(..)
, System(System), Generic(Generic)
, ExpectedResponseForRole, ExpectedResponseForRole1

, Message, MessageT
, StartMessage(StartMessage), StopMessage(StopMessage)

, AgentId(..), AgentInnerInterface(..)
, AgentCommPriority(..), AgentControl(..)
, AgentFullRef(..), fromFullRef
, AgentThread(..), AgentThreads(..), extractThreads
, forceStopAgent, waitAgent

, AgentBehavior(..), AgentHandleMessages(..)

, AgentCreate(..), AgentDescriptor(..)
, AgentsManager(..), AgentsManagerOps(..)

) where

import Data.Typeable

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

\end{code}
%endif

%if standalone
\begin{document}
%endif

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
  send     :: (Message msg)                                         => ref -> msg    -> IO ()
  ask      :: (Message msg, Message (ExpectedResponse msg))         => ref -> msg    -> IO (ExpectedResponse msg)
  askT     :: (MessageT msg t, MessageT (ExpectedResponse1 msg) t)  => ref -> msg t  -> IO (ExpectedResponse1 msg t)

\end{code}

These messages are handled by the corresponding agent's functions.

\begin{code}

data AgentHandleMessages states = AgentHandleMessages {

\end{code}

\begin{itemize}
  \item react to sent messages (sent with \verb|send|):

> handleMessage :: forall msg i .  ( Message msg
>                                  , AgentInnerInterface i) =>
>                  i -> states -> msg -> IO (),

  \item respond un-typed messages (responding to \verb|ask|):

> respondMessage :: forall msg resp i .  ( Message msg, Message resp
>                                        , ExpectedResponse msg ~ resp
>                                        , AgentInnerInterface i) =>
>                   i -> states -> msg -> IO resp,

  \item respond typed messages (responding to \verb|askT|):


> respondTypedMessage :: forall msg resp t i .  ( MessageT msg t, MessageT resp t
>                                               , ExpectedResponse1 msg t ~ resp t
>                                               , AgentInnerInterface i) =>
>                        i -> states -> msg t -> IO (resp t)
> }

\end{itemize}

The expected response type should be defined for every message that is
intended to get responses.

\begin{code}

type family ExpectedResponse   (msg :: *)         :: *
type family ExpectedResponse1  (msg :: * -> *)    :: * -> *

\end{code}


Restriction for messages is having instances of \verb|Typeable| and \verb|Show|.

\begin{code}

type Message msg     = (Typeable msg, Show msg)
type MessageT msg t  = (Typeable t, Typeable msg, Show (msg t))

data StartMessage  = StartMessage  deriving (Typeable, Show)
data StopMessage   = StopMessage   deriving (Typeable, Show)

\end{code}

Agent interface is used to reference agent-self within behavior definitions.

\begin{code}
class AgentInnerInterface i where  selfRef   :: i -> AgentRef
                                   selfStop  :: i -> IO ()
\end{code}

\subsubsection{Role-depending behavior}
The expected response may depend on agent's \emph{role}.

\begin{code}

class (AgentComm (ref agRole)) => AgentCommRole agRole ref where
  askR  :: (Message msg)     => ref agRole
                             -> msg
                             -> IO (ExpectedResponseForRole agRole msg)
  askRT :: (MessageT msg t)  => ref agRole
                             -> msg t
                             -> IO (ExpectedResponseForRole1 agRole msg t)

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

\begin{code}

data AgentId = AgentId String deriving (Eq, Ord)

instance Show AgentId  where show (AgentId s) = s

\end{code}

Normal agent reference is a container for types of class \verb|AgentComm|.

\begin{code}

data AgentRef = forall ref . (AgentComm ref) => AgentRef ref

instance Show AgentRef where show (AgentRef ref) = show $ agentId ref

\end{code}

There is also a role-dependent reference, that contains some instance of \verb|AgentCommRole|.

\begin{code}

data AgentRef' r = forall ref . (AgentCommRole r ref) => AgentRef' (ref r)

\end{code}


A reference itself provides \verb|AgentComm| interface for the underlying agent.

\begin{code}

instance AgentComm AgentRef where
  agentId  (AgentRef ref)  = agentId ref
  send     (AgentRef ref)  = send ref
  ask      (AgentRef ref)  = ask ref
  askT     (AgentRef ref)  = askT ref

instance (Typeable r) => AgentComm (AgentRef' r) where
  agentId  (AgentRef' ref)  = agentId ref
  send     (AgentRef' ref)  = send ref
  ask      (AgentRef' ref)  = ask ref
  askT     (AgentRef' ref)  = askT ref

\end{code}

The role-dependent reference is also an instance of \verb|AgentCommRole|.

\begin{code}

instance (Typeable r) => AgentCommRole r AgentRef' where
  askR   (AgentRef' ref)  = askR ref
  askRT  (AgentRef' ref)  = askRT ref

\end{code}

Referenced agent's id is used for establishing \verb|Eq| and \verb|Ord|
relations over it.

\begin{code}

instance Eq AgentRef  where AgentRef a == AgentRef b        = agentId a == agentId b
instance Ord AgentRef where AgentRef a `compare` AgentRef b = agentId a `compare` agentId b

instance Eq (AgentRef' r)  where AgentRef' a == AgentRef' b        = agentId a == agentId b
instance Ord (AgentRef' r) where AgentRef' a `compare` AgentRef' b = agentId a `compare` agentId b

\end{code}


\subsubsection{Agent control}
Agents should support \emph{priority messages}, that are processed before any
normal message.

\begin{code}
class (AgentComm ref) => AgentCommPriority ref where
  sendPriority  :: (Message msg)                                         => ref -> msg    -> IO ()
  askPriority   :: (Message msg, Message (ExpectedResponse msg))         => ref -> msg    -> IO (ExpectedResponse msg)
  askTPriority  :: (MessageT msg t, MessageT (ExpectedResponse1 msg) t)  => ref -> msg t  -> IO (ExpectedResponse1 msg t)

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

\begin{code}

data AgentFullRef =  forall ref . (AgentControl ref) =>
                     AgentFullRef ref AgentThreads

\end{code}

Each agent is expected to be composed of two execution threads:
\emph{message handling} and \emph{actions}.

\begin{code}

data AgentThreads = AgentThreads  {  _actThread      :: AgentThread
                                  ,  _messageThread  :: AgentThread
                                  }


fromFullRef (AgentFullRef ref _) = AgentRef ref
extractThreads (AgentFullRef _ (AgentThreads act msg)) = (act, msg)

\end{code}

The information about agent's thread permits checking on its status,
waiting for it to finish or killing it, using the provided \verb|ThreadId|.

\begin{code}

data AgentThread = AgentThread {  _threadId        :: ThreadId
                               ,  _threadFinished  :: IO Bool
                               ,  _waitThread      :: IO ()
                               }


forceStopAgent :: AgentFullRef -> IO ()
forceStopAgent fref = do  _killThread act
                          _killThread msg
    where  (act, msg)   = extractThreads fref
           _killThread  = killThread . _threadId

waitAgent :: AgentFullRef -> IO ()
waitAgent fref = do _waitThread act
                    _waitThread msg
    where  (act, msg)   = extractThreads fref

\end{code}

Just like a normal reference, the full one is compared and tested by
the \verb|AgentId|.

\begin{code}

instance Eq AgentFullRef where
  AgentFullRef a _ == AgentFullRef b _         =  agentId a == agentId b
instance Ord AgentFullRef where
  AgentFullRef a _ `compare` AgentFullRef b _  = agentId a `compare` agentId b

\end{code}

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

\begin{code}

class (AgentControl ag) => AgentCreate from ag where
  createAgent :: from -> IO (ag, AgentFullRef)

\end{code}

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



%if standalone
\end{document}
%endif


%%% Local Variables:
%%% latex-build-command: "LGSTex"
%%% lhs-build-standalone-flag: t
%%% eval: (haskell-indentation-mode)
%%% eval: (interactive-haskell-mode)
%%% End:

