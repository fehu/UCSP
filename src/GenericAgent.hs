-----------------------------------------------------------------------------
--
-- Module      :  GenericAgent
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--


module GenericAgent where

import Data.Typeable

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad

-----------------------------------------------------------------------------

data AgentBehaviour states = AgentBehaviour {
  handleMessages  :: AgentHandleMessages states,
  act             :: states -> IO ()
--  onStart         :: states -> IO (),
--  onStop          :: states -> IO ()
}


data AgentHandleMessages states = AgentHandleMessages
  {  handleMessage        ::  forall msg . Typeable msg => states -> msg -> IO ()
  ,  respondMessage       ::  forall msg resp . (ExpectedResponse msg ~ resp) =>
                                    states -> msg -> IO resp
  ,  respondTypedMessage  ::  forall msg resp t . (ExpectedResponse1 msg t ~ resp t) =>
                                    states -> msg t -> IO (resp t)
  }

-----------------------------------------------------------------------------

-- System role.
data System = System
-- Generic role.
data Generic = Generic

-----------------------------------------------------------------------------

class (Typeable ref, Ord ref) => AgentComm ref where
  type AgentRole ref :: *

  agentId   :: ref -> AgentId
  send      :: (Typeable msg)              => ref -> msg    -> IO ()
  ask       :: (Typeable msg)              => ref -> msg    -> IO (ExpectedResponse msg)
  askT      :: (Typeable t, Typeable msg)  => ref -> msg t  -> IO (ExpectedResponse1 msg t)

  -- role-dependent
  askR      :: (Typeable msg)              => ref -> msg    -> IO (ExpectedResponseForRole (AgentRole ref) msg)
  askRT     :: (Typeable t, Typeable msg)  => ref -> msg t  -> IO (ExpectedResponseForRole1 (AgentRole ref) msg t)


newtype AgentId = AgentId String deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------

data AgentRef = forall ref . (AgentComm ref) => AgentRef ref

instance Eq AgentRef  where AgentRef a == AgentRef b        = agentId a == agentId b
instance Ord AgentRef where AgentRef a `compare` AgentRef b = agentId a `compare` agentId b


instance AgentComm AgentRef where
  agentId  (AgentRef ref)  = agentId ref
  send     (AgentRef ref)  = send ref
  ask      (AgentRef ref)  = ask ref
  askT     (AgentRef ref)  = askT ref

-- -----------------------------------------------

type family ExpectedResponse   (msg :: *)         :: *
type family ExpectedResponse1  (msg :: * -> *)    :: * -> *

type family ExpectedResponseForRole   r (msg :: *)         :: *
type family ExpectedResponseForRole1  r (msg :: * -> *)    :: * -> *

-- -----------------------------------------------

class (AgentComm ref) => AgentCommPriority ref where
  sendPriority  :: (Typeable msg)              => ref -> msg    -> IO ()
  askPriority   :: (Typeable msg)              => ref -> msg    -> IO (ExpectedResponse msg)
  askTPriority  :: (Typeable t, Typeable msg)  => ref -> msg t  -> IO (ExpectedResponse1 msg t)


class (AgentCommPriority ag) => AgentControl ag where
    startAgent   :: ag -> IO ()
    stopAgent    :: ag -> IO ()
    stopAgentNow :: ag -> IO ()


-- -----------------------------------------------

class (AgentControl ag) => AgentCreate from ag where
    createAgent :: from -> IO (ag, AgentThreads)

-- destroyed at Stop, so dispose of any ThreadId
data AgentThreads = AgentThreads  {  _actThread      :: AgentThread
                                  ,  _messageThread  :: AgentThread
                                  }

data AgentThread = AgentThread {  _threadId        :: ThreadId
                               ,  _threadFinished  :: IO Bool
                               ,  _waitThread      :: IO ()
                               }

-- -----------------------------------------------

class AgentsManager m where
    newAgentsManager :: IO m
    listAgents       :: m -> IO [AgentFullRef]
    registerAgent    :: m -> AgentFullRef -> IO ()
    unregisterAgent  :: m -> AgentFullRef -> IO ()

    agentsStopped :: m -> IO Bool
    agentsStopped = fmap (foldr (||) True) . _mapEachThread _threadFinished

    waitAllAgents :: m -> IO ()
    waitAllAgents = void . _mapEachThread _waitThread

    foreachAgent :: m -> (AgentFullRef -> IO a) -> IO [a]
    foreachAgent m f =  mapM f =<< listAgents m

    sendEachAgent :: (Typeable msg) => m -> msg -> IO ()
    sendEachAgent m msg = void $ foreachAgent m (`send` msg)

    orderEachAgent :: (Typeable msg) => m -> msg -> IO ()
    orderEachAgent m msg = void $ foreachAgent m (`send` msg)

    createWithManager :: (AgentCreate from ag) => m -> from -> IO (ag, AgentFullRef)
    createWithManager m from = do (ag, threads) <- createAgent from
                                  let ref = AgentFullRef ag threads
                                  m `registerAgent` ref
                                  return (ag, ref)


_mapEachThread :: (AgentsManager m) => (AgentThread -> IO a) -> m -> IO [a]
_mapEachThread f = (mapM f . concatMap (pair2List . extractThreads)) <=< listAgents
    where pair2List (a, b) = [a, b]


-- -----------------------------------------------

data AgentFullRef = forall ref . (AgentControl ref) => AgentFullRef ref AgentThreads

fromFullRef (AgentFullRef ref _) = AgentRef ref

extractThreads (AgentFullRef _ (AgentThreads act msg)) = (act, msg)

forceStopAgent :: AgentFullRef -> IO ()
forceStopAgent fref = do  _killThread act
                          _killThread msg
    where  (act, msg)   = extractThreads fref
           _killThread  = killThread . _threadId


instance Eq AgentFullRef  where AgentFullRef a _ == AgentFullRef b _        = agentId a == agentId b
instance Ord AgentFullRef where AgentFullRef a _ `compare` AgentFullRef b _ = agentId a `compare` agentId b

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

-- -----------------------------------------------

data StartMessage  = StartMessage  deriving Typeable -- Starts agent's act thread
data StopMessage   = StopMessage   deriving Typeable -- Terminates agent

-- -----------------------------------------------


data AgentDescriptor states = AgentDescriptor{
    agentBehaviour  :: AgentBehaviour states,
    newAgentStates  :: IO states,
    nextAgentId     :: IO AgentId
    }


