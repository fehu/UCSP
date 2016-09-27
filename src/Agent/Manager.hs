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

, SimpleAgentsManager, StatelessAgentsManager

, ManagerAgent(..), AgentsManagerRole(..)
, ManagerAgentDescriptor(..), newAgentsManagerAgent

, EmptyResult(..)

, CreateAgent(..), CreateAgents(..), AgentsCreated(..)

, handleCreateAgents, responseCreateAgents
, handleStart, handleStop
)
where

import Agent
import Agent.Extra

import Data.Function (on)
import Data.Typeable
import Data.Maybe (fromJust)
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

instance (EmptyResult res) => AgentsManager (ManagerAgent s res) s where
    newAgentsManager   = flip newAgentsManagerAgent emptyResult
                       =<< emptyManagerAgentDescriptor
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


data ManagerAgentDescriptor = ManagerAgentDescriptor
  {  managerAct_ :: forall i . AgentInnerInterface i => i -> IO ()
  ,  aManagerIdPrefix_  :: String
  ,  aManagerCount_     :: TVar Int
  ,  amHandleMessage_    :: forall msg . Message msg => [msg -> Maybe (IO ())]
  ,  amRespondMessage_   :: forall msg resp . ( Message msg, Message resp
                                              , ExpectedResponse msg ~ resp )
                                           => [msg -> Maybe (IO resp)]
  }

managerAgentDescriptor :: ManagerAgentDescriptor -> res -> AgentDescriptor () res
managerAgentDescriptor descr noResult_ =
    AgentDescriptor{
      newAgentStates = return ()
    , nextAgentId  = const $ do i <- atomically $ do aManagerCount_ descr `modifyTVar` (+1)
                                                     readTVar $ aManagerCount_ descr
                                return . AgentId $ aManagerIdPrefix_ descr ++ show i
    , agentBehaviour = AgentBehavior{
        act = \i _ -> managerAct_ descr i,
        handleMessages = AgentHandleMessages{
          handleMessage = \i states -> selectMessageHandler $ amHandleMessage_ descr,
          respondMessage = \i states -> selectResponse $ amRespondMessage_ descr
          }
      }
    , noResult = noResult_
    }

emptyManagerAgentDescriptor :: IO ManagerAgentDescriptor
emptyManagerAgentDescriptor = do
    cnt <- newTVarIO 0
    return ManagerAgentDescriptor{
      managerAct_ = const $ return (),
      aManagerIdPrefix_ = "EmptyManagerAgent_",
      aManagerCount_ = cnt,
      amHandleMessage_ = [],
      amRespondMessage_ = []
      }

newAgentsManagerAgent :: ManagerAgentDescriptor -> res -> IO (ManagerAgent s res)
newAgentsManagerAgent descr noResult_ = do
    let d =  managerAgentDescriptor descr noResult_
    (_ :: AgentRunOfRole AgentsManagerRole, ref) <- createAgent d
    m <- newAgentsManager
    return $ ManagerAgent ref m noResult_



-- -----------------------------------------------

data CreateAgent states res ag s = CreateAgent{
    crAgDescriptor  :: AgentDescriptor states res,
    crAgExtState    :: ag -> s
    }

data CreateAgents = forall states res ag s . ( Typeable ag, Typeable s, Show s
                                             , AgentCreate (AgentDescriptor states res) ag ) =>
     CreateAgents [CreateAgent states res ag s]
    deriving Typeable

data AgentsCreated = forall s . (Typeable s, Show s) =>
     AgentsCreated [(AgentFullRef, s)]
    deriving Typeable

type instance ExpectedResponse CreateAgents = AgentsCreated

instance Show CreateAgents where
    show (CreateAgents fs) = "CreateAgents " ++ show (length fs)

instance Show AgentsCreated where
    show (AgentsCreated as)  = "AgentsCreated failed:" ++ show as


responseCreateAgents  :: ( resp ~ ExpectedResponse msg
                         , Message msg, Message resp
                         , AgentsManager m s
                         , Typeable s, Show s
                         )
                      => m -> msg -> Maybe (IO resp)
responseCreateAgents m = mbResp $ \(CreateAgents cas) -> createAgents_ cas m

handleCreateAgents  :: ( Message msg, AgentsManager m s
                       , Typeable s, Show s )
                    => m -> msg -> Maybe (IO ())
handleCreateAgents m = mbHandle $ \(CreateAgents cas) -> void $ createAgents_ cas m

createAgents_ cas manager =
        liftM AgentsCreated $
        sequence =<<
        forM cas (
          \(CreateAgent d sf) -> return $
            do (ag, ref) <- createAgent d
               let eState = fromJust . cast $ sf ag
               manager `registerAgent` (ref, eState)
               return (ref, eState)
               )


-- -----------------------------------------------

handleStart onStart  = mbHandle $ \StartMessage  -> onStart
handleStop onStop    = mbHandle $ \StopMessage   -> onStop

-- -----------------------------------------------


