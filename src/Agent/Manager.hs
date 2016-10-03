-----------------------------------------------------------------------------
--
-- Module      :  Agent.Manager
-- Copyright   :
-- License     :  MIT
--
-- |
--
-----------------------------------------------------------------------------

module Agent.Manager (
  AgentsManager(..)
, AgentsManagerOps (..)
--, SomeAgentsManager(..)

, SimpleAgentsManager, StatelessAgentsManager

, ManagerAgent(..), AgentsManagerRole(..)
, ManagerAgentDescriptor(..)
, generalizeManagerDescriptor
, CreateAgentsManager(..)

, EmptyResult(..)

--, handleStart, handleStop
)
where

import Agent
import Agent.Extra

import Data.Function (on)
import Data.Typeable
import Data.List (delete)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad
import Control.Exception
import Control.Concurrent.STM


-- \subsubsection{Agent Management}
-- | A manager registers/unregisters agent references and provides
--   agent-related operations over them.


class AgentsManager m s | m -> s where
  newAgentsManager :: IO m
  listAgents       :: m -> IO [AgentFullRef]
  listAgentStates  :: m -> IO [(AgentFullRef, s)]
  registerAgent    :: m -> (AgentFullRef, s) -> IO ()
  unregisterAgent  :: m -> AgentFullRef -> IO ()

  mapAgents        :: (AgentFullRef -> IO a)   -> m -> IO [a]
  mapAgents_       :: (AgentFullRef -> IO ())  -> m -> IO ()

  mapAgentStates   :: ((AgentFullRef, s) -> IO a)   -> m -> IO [a]
  mapAgentStates_  :: ((AgentFullRef, s) -> IO ())  -> m -> IO ()

  foreachAgent     :: m -> (AgentFullRef -> IO a)   -> IO [a]
  foreachAgent_    :: m -> (AgentFullRef -> IO ())  -> IO ()

  foreachAgentState   :: m -> ((AgentFullRef, s)  -> IO a)   -> IO [a]
  foreachAgentState_  :: m -> ((AgentFullRef, s)  -> IO ())  -> IO ()


  mapAgents f   = mapM f   <=< listAgents
  mapAgents_ f  = mapM_ f  <=< listAgents
  mapAgentStates f   = mapM f   <=< listAgentStates
  mapAgentStates_ f  = mapM_ f  <=< listAgentStates

  foreachAgent   = flip mapAgents
  foreachAgent_  = flip mapAgents_
  foreachAgentState   = flip mapAgentStates
  foreachAgentState_  = flip mapAgentStates_

  createWithManager  :: ( AgentCreate from ag
                        , AgentsManager m s)   => m
                                               -> from
                                               -> (ag -> IO s)
                                               -> IO (ag, (AgentFullRef, s))


class AgentsManagerOps m where
  agentsStopped      :: m -> IO Bool
  waitAllAgents      :: m -> IO ()

  sendEachAgent      :: (Message msg) => m -> msg -> IO ()
  orderEachAgent     :: (Message msg) => m -> msg -> IO ()


--data SomeAgentsManager s = forall m . (AgentsManager m s, AgentsManagerOps m) =>
--     SomeAgentsManager m

--instance AgentsManager (SomeAgentsManager s) s where
--    newAgentsManager = undefined -- TODO ?? no ops instance
--    listAgents (SomeAgentsManager s) = listAgents s
--    listAgentStates (SomeAgentsManager s) = listAgentStates s
--    registerAgent (SomeAgentsManager s) = registerAgent s
--    unregisterAgent (SomeAgentsManager s) = unregisterAgent s
--    createWithManager (SomeAgentsManager s) = createWithManager s
--
--instance AgentsManagerOps (SomeAgentsManager s) where
--    agentsStopped (SomeAgentsManager s) = agentsStopped s
--    waitAllAgents (SomeAgentsManager s) = waitAllAgents s
--    sendEachAgent (SomeAgentsManager s) = sendEachAgent s
--    orderEachAgent (SomeAgentsManager s) = orderEachAgent s

-- -----------------------------------------------
-- -----------------------------------------------

data SimpleAgentsManager s = SimpleAgentsManager { registeredAgents :: TVar (Map AgentFullRef s) }

instance AgentsManager (SimpleAgentsManager s) s where
    newAgentsManager = SimpleAgentsManager <$> newTVarIO Map.empty

    listAgents      = fmap Map.keys   . readTVarIO . registeredAgents
    listAgentStates = fmap Map.assocs . readTVarIO . registeredAgents

    registerAgent m (ref, s)  = atomically $ registeredAgents m `modifyTVar` Map.insert ref s
    unregisterAgent m ref     = atomically $ registeredAgents m `modifyTVar` Map.delete ref

    createWithManager m from mkState = do  (ag, ref)  <- createAgent from
                                           state      <- mkState ag
                                           m `registerAgent` (ref, state)
                                           return (ag, (ref, state))


instance AgentsManagerOps (SimpleAgentsManager s) where
    agentsStopped  = fmap (foldr (||) True) . _mapEachThread _threadFinished
    waitAllAgents  = void . _mapEachThread _waitThread

    sendEachAgent m msg   = void $ foreachAgent m (`send` msg)
    orderEachAgent m msg  = void $ foreachAgent m (`sendPriority` msg)


_mapEachThread :: (AgentsManager m s) => (AgentThread -> IO a) -> m -> IO [a]
_mapEachThread f = (mapM f . concatMap (pair2List . extractThreads)) <=< listAgents
    where pair2List (a, b) = [a, b]

-- -----------------------------------------------

type StatelessAgentsManager = SimpleAgentsManager ()


-- -----------------------------------------------
-- -----------------------------------------------

data ManagerAgent s res = ManagerAgent AgentFullRef (SimpleAgentsManager s) res
    deriving Typeable

data AgentsManagerRole = AgentsManagerRole

managerAgentRef    (ManagerAgent ref _ _)  = ref
managerAgentM_     (ManagerAgent _ m _)    = m
-- managerAgentNoRes  (ManagerAgent _ _ res)  = res

instance Eq (ManagerAgent s res)   where (==) = (==) `on` managerAgentRef
instance Ord (ManagerAgent s res)  where compare = compare `on` managerAgentRef
instance (Typeable s, Typeable res) => AgentComm (ManagerAgent s res) where
    agentId  = agentId . managerAgentRef
    send     = send . managerAgentRef
    ask      = ask . managerAgentRef
instance (Typeable s, Typeable res) => AgentCommPriority (ManagerAgent s res) where
    sendPriority  = sendPriority . managerAgentRef
    askPriority   = askPriority . managerAgentRef

instance (EmptyResult res, Typeable res, Typeable s) => AgentsManager (ManagerAgent s res) s where
    newAgentsManager   = newAgentsManagerAgent =<< emptyManagerAgentDescriptor
    listAgents         = listAgents . managerAgentM_
    listAgentStates    = listAgentStates . managerAgentM_
    registerAgent      = registerAgent . managerAgentM_
    unregisterAgent    = unregisterAgent . managerAgentM_
    createWithManager  = createWithManager . managerAgentM_
instance AgentsManagerOps (ManagerAgent s res) where
    agentsStopped   = agentsStopped . managerAgentM_
    waitAllAgents   = waitAllAgents . managerAgentM_
    sendEachAgent   = sendEachAgent . managerAgentM_
    orderEachAgent  = orderEachAgent . managerAgentM_

class EmptyResult a where emptyResult :: a


data ManagerAgentDescriptor s = ManagerAgentDescriptor
  {  managerBehaviour   :: AgentBehavior s
  ,  aManagerIdPrefix_  :: String
  ,  aManagerCount_     :: TVar Int
  ,  debugManager       :: Bool
  }

generalizeManagerDescriptor :: ManagerAgentDescriptor m
                            -> res
                            -> AgentDescriptor m res
generalizeManagerDescriptor descr noResult_ =
    AgentDescriptor{
      newAgentStates = return undefined
    , nextAgentId  = const $ do i <- atomically $ do aManagerCount_ descr `modifyTVar` (+1)
                                                     readTVar $ aManagerCount_ descr
                                return . AgentId $ aManagerIdPrefix_ descr ++ show i
    , agentDefaultBehaviour = managerBehaviour descr
    , noResult = noResult_
    , debugAgent = debugManager descr
    }

emptyManagerAgentDescriptor :: IO (ManagerAgentDescriptor s)
emptyManagerAgentDescriptor = do
    cnt <- newTVarIO 0
    return ManagerAgentDescriptor{
      managerBehaviour = agentNoBehaviour,
      aManagerIdPrefix_ = "SomeManagerAgent_",
      aManagerCount_ = cnt,
      debugManager = False
      }


class CreateAgentsManager m s res | m -> s, m -> res
  where
    newAgentsManagerAgent :: ManagerAgentDescriptor s -> IO m


instance (EmptyResult res, Typeable s) =>
  CreateAgentsManager (ManagerAgent s res) s res where
    newAgentsManagerAgent descr = do
        let d =  generalizeManagerDescriptor descr (emptyResult :: res)
        (_ :: AgentRunOfRole AgentsManagerRole, ref) <- createAgent d
        m <- newAgentsManager
        return $ ManagerAgent ref m emptyResult

--newAgentsManagerAgent :: ( Typeable s, Typeable res
--                         , AgentsManager m (s res), AgentsManagerOps m ) =>
--                         ManagerAgentDescriptor (s res)
--                      -> res -> IO m -- (ManagerAgent s res)
--newAgentsManagerAgent descr noResult_ = do
--    let d =  generalizeManagerDescriptor descr noResult_
--    (_ :: AgentRunOfRole AgentsManagerRole, ref) <- createAgent d
--    m <- newAgentsManager
--    return $ ManagerAgent ref m noResult_

-- -----------------------------------------------

-- handleStart onStart  = mbHandle $ \StartMessage  -> onStart
-- handleStop onStop    = mbHandle $ \StopMessage   -> onStop

-- -----------------------------------------------


