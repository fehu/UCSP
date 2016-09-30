%if standalone
\documentclass{article}

%include src/Document/format.fmt
%include polycode.fmt
%include forall.fmt

\usepackage[english]{babel}
\usepackage[inline, shortlabels]{enumitem}

\usepackage{showframe}

%endif

%if False
\begin{code}

module Agent.Abstract(

  AgentAct, AgentBehavior(..)
, AgentHandleMessages(..)

, AgentComm(..), ExpectedResponse
, AgentRef(..), AgentRef'(..), simpleRef

, AgentCommRole(..), ExpectedResponseForRole
, System(System), Generic(Generic)

, Message
, StartMessage(StartMessage), StopMessage(StopMessage)

, AgentExecControl(..), AgentInnerInterface
, AgentActControl(..), Millis

, AgentId(..)
, AgentCommPriority(..), AgentControl(..)
, AgentFullRef(..)
, AgentThread(..), AgentThreads(..)

, selectMessageHandler, mbHandle
, selectResponse, mbResp

, AgentCreate(..), AgentDescriptor(..)

) where

import Data.Typeable
import Data.Maybe (fromMaybe)

import Control.Applicative ((<|>))
import Control.Monad
import Control.Concurrent (ThreadId)
import Control.Concurrent.STM

import GHC.Exts (Constraint)

\end{code}
%endif

%if standalone
\begin{document}
%endif

An agent is represented by two processes: active actions (not caused by external reason)
and messages handling. Both are defined flexibly and can be changed during the execution through
'AgentExecControl' or 'AgentInnerInterface'. It is also used to reference agent-self within behavior definitions.

\begin{code}

type AgentAct states = forall i . (AgentExecControl i states) => i -> states -> IO ()

type Millis = Int -- Milliseconds

class AgentExecControl i states | i -> states where
    agentRef        :: i -> AgentRef
    agentTerminate  :: i -> IO ()

    actOnce    :: i -> AgentAct states -> IO ()
    actRepeat  :: i -> AgentAct states -> Maybe Millis -> IO ()
    actPause   :: i -> IO ()

    setMessageHandlers  :: i -> AgentHandleMessages states -> IO ()
    setAgentBehavior    :: i -> AgentBehavior states -> IO ()

    agentDebug  :: i -> Bool
    whenDebug   :: i -> IO () -> IO ()
    debugMsg    :: Show a => i -> a -> IO ()

    whenDebug i  = when (agentDebug i)
    debugMsg i   = whenDebug i . putStrLn . (debugPref ++) . show
                 where debugPref = "[DEBUG][" ++ show (agentId $ agentRef i) ++ "]"

-- Alias for AgentExecControl
type AgentInnerInterface = AgentExecControl

\end{code}

Agent also provides complex actions:
\begin{code}
data AgentActControl states =
\end{code}
\begin{itemize}
  \item actions sequence;

>    AgentActOnce    (AgentAct states) (AgentActControl states) |

  \item repeating action;

>    AgentActRepeat  (AgentAct states) (Maybe Millis) |

  \item no action.

>    AgentNoAct

\end{itemize}


Agent's behavior is defined by its \emph{action loop} and incoming
\emph{messages handling}.

\begin{code}

data AgentBehavior states = AgentBehavior {
  agentAct        :: AgentActControl states,
  handleMessages  :: AgentHandleMessages states
  }


\end{code}


\subsubsection{Behavior definition}


Messages can be just \emph{sent} to any agent or a specific \emph{response} may
be \emph{asked}.
\begin{code}

class (Typeable ref, Ord ref) => AgentComm ref where
  agentId  :: ref -> AgentId
  send     :: (Message msg)                                 => ref -> msg  -> IO ()
  ask      :: (Message msg, Message (ExpectedResponse msg)) => ref -> msg  -> IO (ExpectedResponse msg)

\end{code}

These messages are handled by the corresponding agent's functions.

\begin{code}

data AgentHandleMessages states = AgentHandleMessages {

\end{code}

\begin{itemize}
  \item react to sent messages (sent with \verb|send|):

> handleMessage :: forall msg i .  ( Message msg
>                                  , AgentInnerInterface i states) =>
>                  i -> states -> msg -> IO (),

  \item respond un-typed messages (responding to \verb|ask|):

> respondMessage :: forall msg resp i .  ( Message msg, Message resp
>                                        , ExpectedResponse msg ~ resp
>                                        , AgentInnerInterface i states) =>
>                   i -> states -> msg -> IO resp
> }

\end{itemize}

The expected response type should be defined for every message that is
intended to get responses.

\begin{code}

type family ExpectedResponse (msg :: *) :: *

\end{code}

Helper functions for handling messages.

\begin{code}

mbHandle  :: (Message msg0, Message msg)
          => (msg -> IO ()) -> msg0 -> Maybe (IO ())
mbHandle f msg = f <$> cast msg

selectMessageHandler :: Message msg => [msg -> Maybe (IO ())] -> msg -> IO ()
selectMessageHandler rfs msg = fromMaybe failed  $ foldr (<|>) Nothing
                                                 $ ($ msg) <$> rfs
    where failed = fail $ "Couldn't match received message: " ++ show msg

\end{code}


Helper functions for building responses.

\begin{code}

mbResp  :: ( ExpectedResponse msg0 ~ resp0 , Message msg0, Message resp0
           , ExpectedResponse msg ~ resp , Message msg, Message resp
           )
        => (msg -> IO resp) -> msg0 -> Maybe (IO resp0)
mbResp f msg = cast =<< f <$> cast msg

selectResponse  :: (resp ~ ExpectedResponse msg, Message msg)
                => [msg -> Maybe (IO resp)] -> msg -> IO resp
selectResponse rfs msg = fromMaybe failed  $ foldr (<|>) Nothing
                                           $ ($ msg) <$> rfs
    where failed = fail $ "Couldn't match received message: " ++ show msg

\end{code}


Restriction for messages is having instances of \verb|Typeable| and \verb|Show|.

\begin{code}

type Message msg     = (Typeable msg, Show msg)

data StartMessage  = StartMessage  deriving (Typeable, Show)
data StopMessage   = StopMessage   deriving (Typeable, Show)

\end{code}


\subsubsection{Role-depending behavior}
The expected response may depend on agent's \emph{role}.

\begin{code}

class (AgentComm (ref agRole)) => AgentCommRole agRole ref where
  askR  :: (Message msg)     => ref agRole
                             -> msg
                             -> IO (ExpectedResponseForRole agRole msg)

type family ExpectedResponseForRole r (msg :: *) :: *


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

simpleRef (AgentRef' ref) = AgentRef ref

\end{code}


A reference itself provides \verb|AgentComm| interface for the underlying agent.

\begin{code}

instance AgentComm AgentRef where
  agentId  (AgentRef ref)  = agentId ref
  send     (AgentRef ref)  = send ref
  ask      (AgentRef ref)  = ask ref

instance (Typeable r) => AgentComm (AgentRef' r) where
  agentId  (AgentRef' ref)  = agentId ref
  send     (AgentRef' ref)  = send ref
  ask      (AgentRef' ref)  = ask ref

\end{code}

The role-dependent reference is also an instance of \verb|AgentCommRole|.

\begin{code}

instance (Typeable r) => AgentCommRole r AgentRef' where
  askR   (AgentRef' ref)  = askR ref

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
  sendPriority  :: (Message msg)                                  => ref -> msg  -> IO ()
  askPriority   :: (Message msg, Message (ExpectedResponse msg))  => ref -> msg  -> IO (ExpectedResponse msg)

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

instance Show AgentFullRef where
    show (AgentFullRef ref _) = "AgentFullRef " ++ show (agentId ref)

\end{code}

Each agent is expected to be composed of two execution threads:
\emph{message handling} and \emph{actions}.

\begin{code}

data AgentThreads = AgentThreads  {  _actThread      :: AgentThread
                                  ,  _messageThread  :: AgentThread
                                  }

\end{code}

The information about agent's thread permits checking on its status,
waiting for it to finish or killing it, using the provided \verb|ThreadId|.

\begin{code}

data AgentThread = AgentThread {  _threadId        :: ThreadId
                               ,  _threadFinished  :: IO Bool
                               ,  _waitThread      :: IO ()
                               }

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

instance AgentCommPriority AgentFullRef where
  sendPriority  (AgentFullRef ref _) = sendPriority ref
  askPriority   (AgentFullRef ref _) = askPriority  ref

instance AgentControl AgentFullRef where
  startAgent   (AgentFullRef ref _) = startAgent ref
  stopAgent    (AgentFullRef ref _) = stopAgent ref
  stopAgentNow (AgentFullRef ref _) = stopAgentNow ref

\end{code}


\subsubsection{Agent Creation}

Generic creation is defined in for types $\mathrm{from}$ and $\mathrm{ag}$.

\begin{code}

class (AgentControl ag) => AgentCreate from ag where
  createAgent   :: from -> IO (ag, AgentFullRef)

\end{code}




A simple \emph{agent descriptor} that can be used for agent creation.

\begin{code}

data AgentDescriptor states result = AgentDescriptor{
  agentDefaultBehaviour  :: AgentBehavior states,
  newAgentStates         :: IO states,
  nextAgentId            :: states -> IO AgentId,
  noResult               :: result,
  debugAgent             :: Bool
  }

instance Show (AgentDescriptor states res) where
    show _ = "*AgentDescriptor*"

instance Show (AgentActControl states) where
    show (AgentActRepeat _ t)  = "AgentActRepeat" ++ maybe ""
                               (\t -> " every " ++ show t ++ "milliseconds") t
    show (AgentActOnce _ a)    = "AgentActOnce and then " ++ show a
    show AgentNoAct            = "AgentNoAct"

\end{code}



%if standalone
\end{document}
%endif


%%% Local Variables:
%%% latex-build-command: "lhsTeX"
%%% lhs-build-standalone-flag: t
%%% eval: (haskell-indentation-mode)
%%% eval: (interactive-haskell-mode)
%%% End:

