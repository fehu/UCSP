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

, SimpleAgentsManager
)
where

import Agent.Abstract
import Agent.Extra

import Data.List (delete)
import Control.Monad
import Control.Concurrent.STM


-- \subsubsection{Agent Management}
-- | A manager registers/unregisters agent references and provides
--   agent-related operations over them.

-- \begin{code}
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

-- \end{code}


-- -----------------------------------------------
-- -----------------------------------------------


data SimpleAgentsManager = SimpleAgentsManager { registeredAgents :: TVar [AgentFullRef] }

instance AgentsManager SimpleAgentsManager where
    newAgentsManager = SimpleAgentsManager <$> newTVarIO []
    listAgents = readTVarIO . registeredAgents
    registerAgent m ref = atomically $ registeredAgents m `modifyTVar` (ref:)
    unregisterAgent m ref = atomically $ registeredAgents m `modifyTVar` delete ref

    mapAgents f   = mapM f   <=< listAgents
    mapAgents_ f  = mapM_ f  <=< listAgents


instance AgentsManagerOps SimpleAgentsManager where
    agentsStopped  = fmap (foldr (||) True) . _mapEachThread _threadFinished
    waitAllAgents  = void . _mapEachThread _waitThread

    sendEachAgent m msg   = void $ foreachAgent m (`send` msg)
    orderEachAgent m msg  = void $ foreachAgent m (`sendPriority` msg)

    createWithManager m from = do (ag, ref) <- createAgent from
                                  m `registerAgent` ref
                                  return (ag, ref)




_mapEachThread :: (AgentsManager m) => (AgentThread -> IO a) -> m -> IO [a]
_mapEachThread f = (mapM f . concatMap (pair2List . extractThreads)) <=< listAgents
    where pair2List (a, b) = [a, b]


