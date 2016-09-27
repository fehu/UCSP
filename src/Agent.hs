-----------------------------------------------------------------------------
--
-- Module      :  Agent
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Agent (
  AgentRunOfRole
, SystemAgent, GenericAgent

, whenM

, module A

) where

import Agent.Abstract as A

import Data.Typeable
import Data.Function (on)
import Data.Maybe
-- import Data.List (delete)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad

-- -----------------------------------------------

type SystemAgent  = AgentRunOfRole System
type GenericAgent = AgentRunOfRole Generic

-- -----------------------------------------------

data Message' = forall msg . Message msg  => Message msg
instance Show Message' where show (Message msg) = show msg

-- -----------------------------------------------

data AgentRun r states = AgentRun {
  _agentId             :: AgentId,
  _states              :: states,
  _runState            :: TVar RunState,
  _messageBox          :: TQueue (Either Message' (MessageWithResponse r)),
  _messageBoxPriority  :: TQueue (Either Message' (MessageWithResponse r)),
  _agentBehaviour      :: AgentBehavior states
  }

data MessageWithResponse r =
    forall msg resp . (Message msg, Message resp, ExpectedResponse msg ~ resp) =>
        MessageWithResponse msg (resp -> IO())

  | forall msg resp . (Message msg, Message resp, ExpectedResponseForRole r msg ~ resp) =>
        MessageWithResponse' msg (resp -> IO())


data RunState = Created | Running | Terminate deriving (Show, Eq)


instance Show (MessageWithResponse r) where
    show (MessageWithResponse    msg _) = show msg
    show (MessageWithResponse'   msg _) = show msg

-- -----------------------------------------------

data AgentRunOfRole r = forall states . Typeable states => AgentRunOfRole (AgentRun r states)
agentRunOfRoleId (AgentRunOfRole run) = _agentId run

instance Eq (AgentRunOfRole r)   where (==)     = (==) `on` agentRunOfRoleId
instance Ord (AgentRunOfRole r)  where compare  = compare `on` agentRunOfRoleId

--extractAgentState f (AgentRunOfRole ag) = f <$> cast (_states ag)

-- -----------------------------------------------


instance (Typeable r) => AgentComm (AgentRunOfRole r) where
    agentId (AgentRunOfRole run)  = _agentId run
    send (AgentRunOfRole run)     = _writeTQueue run _messageBox . Left . Message
    ask   = _ask MessageWithResponse


_writeTQueue run getBox msg' = atomically $ writeTQueue (getBox run) msg'

_ask mkHook (AgentRunOfRole run) msg = do
        respVar <- newEmptyMVar
        _writeTQueue run _messageBox . Right $ mkHook msg (putMVar respVar)
        readMVar respVar


-- -----------------------------------------------

instance (Typeable r) => AgentCommPriority (AgentRunOfRole r) where
    sendPriority (AgentRunOfRole run) = _writeTQueue run _messageBoxPriority . Left . Message
    askPriority   = _askPriority MessageWithResponse

_askPriority mkHook (AgentRunOfRole run) msg = do
        respVar <- newEmptyMVar
        _writeTQueue run _messageBoxPriority . Right $ mkHook msg (putMVar respVar)
        readMVar respVar


instance (Typeable r) => AgentControl (AgentRunOfRole r) where
    startAgent ag   = ag `sendPriority` StartMessage
    stopAgent ag    = ag `send` StopMessage
    stopAgentNow ag = ag `sendPriority` StopMessage


-- -----------------------------------------------


whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb a = (`when` a) =<< mb


_runAgentMessages (AgentRunOfRole ag) = do
    msg <- atomically $ do  priority  <- tryReadTQueue $ _messageBoxPriority ag
                            runState  <- readTVar $ _runState ag
                            msg  <- case (priority, runState) of
                                        (Nothing, Running)  -> tryReadTQueue $ _messageBox ag
                                        _                   -> return priority
                            if runState == Terminate  then fail "Terminated"
                                                      else maybe retry return msg
    let  h       = handleMessages $ _agentBehaviour ag
         states  = _states ag

    putStrLn $ "Message: " ++ show msg
    case msg of  Left (Message msg) ->
                        let  mbStart  = (\StartMessage  -> do putStrLn "Start message received"
                                                              ag `_start` states
                                        )  <$> cast msg
                             mbStop   = (\StopMessage   -> do putStrLn "Stop message received"
                                                              ag `_stop` states
                                        )  <$> cast msg
                        in fromMaybe (handleMessage h ag states msg) $ mbStart <|> mbStop
                 Right (MessageWithResponse msg respond) ->
                        respond =<< respondMessage h ag states msg

_run  :: (RunState -> Bool)
      -> (AgentRun r states -> states -> STM ())
      -> AgentRun r states -> states
      -> IO ()
_run atRunState action ag states =
    atomically $ whenM (fmap atRunState . readTVar $ _runState ag)
                       (action ag states)

-- runs `_act` the corresponding thread thread
_start = _run (Created ==) $ \ag states -> _runState ag `writeTVar` Running
                                    -- do  onStart (_agentBehaviour ag) states

_stop = _run (const True) $ \ag _ -> _runState ag `writeTVar` Terminate


_runAgent (AgentRunOfRole ag) = do
    runState <- atomically . readTVar $ _runState ag
    case runState of  Terminate  -> fail "Terminated"
                      Created    -> return ()
                      Running    -> act (_agentBehaviour ag) ag (_states ag)


instance (Typeable r, Typeable states) => AgentInnerInterface (AgentRun r states) where
    selfRef  arun  = AgentRef (AgentRunOfRole arun)
    selfStop arun  = atomically $ _runState arun `writeTVar` Terminate

-- -----------------------------------------------
-- -----------------------------------------------

instance (Typeable r, Typeable states) => AgentCreate (AgentDescriptor states) (AgentRunOfRole r)
  where
    createAgent AgentDescriptor  {  agentBehaviour=behaviour
                                 ,  newAgentStates=newStates
                                 ,  nextAgentId=nextId } =
        do  id        <- nextId undefined
            states    <- newStates
            runState  <- newTVarIO Created

            messageBoxPriority  <- newTQueueIO
            messageBox          <- newTQueueIO

            let  run   = AgentRun id states runState messageBox messageBoxPriority behaviour
                 run'  = AgentRunOfRole run

            msgThreadStopped <- newEmptyMVar
            actThreadStopped <- newEmptyMVar

            -- Start threads
            msgThreadId  <- forkFinally  (forever $ _runAgentMessages run')
                                         (\_ -> do msgThreadStopped `putMVar` undefined
                                                   putStrLn "Message Thread Terminated"
                                                )
            actThreadId  <- forkFinally  (forever $ _runAgent run')
                                         (\_ -> do actThreadStopped `putMVar` undefined
                                                   putStrLn "Act Thread Terminated"
                                         )

            let msgThread = AgentThread {  _threadId       = msgThreadId
                                        ,  _threadFinished = not <$> isEmptyMVar msgThreadStopped
                                        ,  _waitThread     = readMVar msgThreadStopped
                                        }
                actThread = AgentThread {  _threadId       = actThreadId
                                        ,  _threadFinished = not <$> isEmptyMVar actThreadStopped
                                        ,  _waitThread     = readMVar actThreadStopped
                                        }
                threads = AgentThreads actThread msgThread

            return (run', AgentFullRef run' threads)






