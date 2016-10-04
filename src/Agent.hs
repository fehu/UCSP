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

, agentNoBehaviour

, extractAgentStates

, module A

) where

import Agent.Abstract as A

import Data.Typeable
import Data.Function (on)
import Data.Maybe

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
  _agentId :: AgentId,

  _actCtrl  :: TVar (AgentActControl states),
  _msgCtrl  :: TVar (AgentHandleMessages states),

  _actUpdateCtrl   :: TMVar (AgentActControl states),

  _runState        :: TVar RunState,
  _runStateUpdate  :: TMVar RunState,

  _states    :: states,

  _messageBox          :: TQueue (Either Message' (MessageWithResponse r)),
  _messageBoxPriority  :: TQueue (Either Message' (MessageWithResponse r)),

  _agentDebug  :: Bool
  }

getRunState = readTVar . _runState

updRunState ag s = do  debugMsg ag $ "updating Run state: " ++ show s
                       atomically  $ _runStateUpdate ag `putTMVar` s


extractAgentStates :: Typeable s => AgentRunOfRole r -> Maybe s
extractAgentStates (AgentRunOfRole ag) = cast $ _states ag


data MessageWithResponse r =
    forall msg resp . (Message msg, Message resp, ExpectedResponse msg ~ resp) =>
        MessageWithResponse msg (resp -> IO())

  | forall msg resp . (Message msg, Message resp, ExpectedResponseForRole r msg ~ resp) =>
        MessageWithResponse' msg (resp -> IO())


data RunState = Paused | Running | Terminate deriving (Show, Eq)


instance Show (MessageWithResponse r) where
    show (MessageWithResponse    msg _) = show msg
    show (MessageWithResponse'   msg _) = show msg

-- -----------------------------------------------

data AgentRunOfRole r = forall states . Typeable states => AgentRunOfRole (AgentRun r states)
agentRunOfRoleId (AgentRunOfRole run) = _agentId run

instance Eq (AgentRunOfRole r)   where (==)     = (==) `on` agentRunOfRoleId
instance Ord (AgentRunOfRole r)  where compare  = compare `on` agentRunOfRoleId

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


_updateStates ag = do  mbAct  <- tryTakeTMVar $ _actUpdateCtrl ag
                       mbRun  <- tryTakeTMVar $ _runStateUpdate ag

                       let mb = flip $maybe (return ())
                       mb mbAct (_actCtrl ag `writeTVar`)
                       mb mbRun (_runState ag `writeTVar`)


-- | Waits update of the state or `upd` variable.
--   Applies the update to agent's variables before returning
--   `Left var update` or `Right run state update`.
_waitUpdate ag upd var =    (lf =<< takeTMVar (upd ag))
                       <|>  (rf =<< takeTMVar (_runStateUpdate ag))
    where  lf = fmap Left  . sideEffect (var ag `writeTVar`)
           rf = fmap Right . sideEffect (_runState ag `writeTVar`)
           sideEffect f x = f x >> return x

-- | Handles messages as defiend in _msgCtrl.
--   Default handlers for 'StartMessage' and 'StopMessage'
--   are predefined, they change agent's act state on 'Running'
--   and 'Terminate' respectfully.

_runAgentMessages :: Typeable r => AgentRunOfRole  r -> IO ()
_runAgentMessages (AgentRunOfRole ag) = do
    msg <- atomically $ do  -- _updateStates ag
                            priority  <- tryReadTQueue $ _messageBoxPriority ag
                            runState  <- getRunState ag
                            msg  <- case (priority, runState) of
                                        (Nothing, Running)  -> tryReadTQueue $ _messageBox ag
                                        _                   -> return priority
                            if runState == Terminate  then fail "Terminated"
                                                      else maybe retry return msg
    h <- readTVarIO $ _msgCtrl  ag
    let states = _states ag
        dprint = debugMsg ag
    dprint $ "Message: " ++ show msg
    case msg of  Left (Message msg) ->
                        let  mbStart  = (\StartMessage  -> do dprint "Start message received"
                                                              ag `_start` states
                                        )  <$> cast msg
                             mbStop   = (\StopMessage   -> do dprint "Stop message received"
                                                              ag `_stop` states
                                        )  <$> cast msg
                        in fromMaybe (handleMessage h ag states msg) $ mbStart <|> mbStop
                 Right (MessageWithResponse msg respond) ->
                        respond =<< respondMessage h ag states msg

_run  :: (Typeable states, Typeable r) => (RunState -> Bool)
      -> (AgentRun r states -> states -> IO ())
      -> AgentRun r states -> states
      -> IO ()
_run atRunState action ag states =
    whenM (atRunState <$> atomically (getRunState ag))
          (action ag states)

-- runs `_act` the corresponding thread thread
_start :: (Typeable states, Typeable r) => AgentRun r states -> states -> IO ()
_start = _run (Paused ==) $ \ag states -> ag `updRunState` Running

_stop :: (Typeable states, Typeable r) => AgentRun r states -> states -> IO ()
_stop = _run (const True) $ \ag _ -> ag `updRunState` Terminate


_runAgent ag'@(AgentRunOfRole ag) = do
    actAndState <- atomically $ (,)  <$> getRunState ag
                                     <*> readTVar (_actCtrl ag)
    let waitActUpd = do  debugMsg ag "act: wait update"
                         upd <- atomically (_waitUpdate ag _actUpdateCtrl _actCtrl)
                         debugMsg ag $  "act: updated: " ++ show upd ++
                                        "; executing runAgent again"
                         _runAgent ag'
        execAct act = act ag (_states ag)

    case actAndState
      of  (Terminate, _)                -> fail "Terminated"
          (Paused, _)                   -> waitActUpd
          (_, AgentNoAct)               -> waitActUpd
          (_, AgentActOnce act after)   -> atomically (_actCtrl ag `writeTVar` after) >> execAct act
          (_, AgentActRepeat act pause) -> execAct act >> maybe (return ()) threadDelay pause


instance (Typeable r, Typeable states) => AgentExecControl (AgentRun r states) states where
    agentRef arun        = AgentRef (AgentRunOfRole arun)
    agentTerminate arun  = arun `updRunState` Terminate

    actOnce arun act = atomically $ do  old <- readTVar $ _actCtrl arun
                                        _actUpdateCtrl arun `putTMVar` AgentActOnce act old
    actRepeat arun act = atomically . putTMVar (_actUpdateCtrl arun) . AgentActRepeat act
    actPause arun = arun `updRunState` Paused

    setMessageHandlers arun = atomically . writeTVar (_msgCtrl arun)
    setAgentBehavior arun b = atomically $ do _msgCtrl arun `writeTVar` handleMessages b
                                              _actUpdateCtrl arun `putTMVar` agentAct b
    agentDebug = _agentDebug

-- -----------------------------------------------
-- -----------------------------------------------

agentNoBehaviour = AgentBehavior AgentNoAct $ AgentHandleMessages (\i s -> selectMessageHandler [])
                                                                  (\i s -> selectResponse [])

instance (Typeable r, Typeable states) => AgentCreate  (AgentDescriptor states res)
                                                       (AgentRunOfRole r)
  where
    createAgent AgentDescriptor  {  agentDefaultBehaviour=behaviour
                                 ,  newAgentStates=newStates
                                 ,  nextAgentId=nextId
                                 ,  debugAgent=debug } =
        do  id        <- nextId undefined
            states    <- newStates

            run <- atomically $
                do  actCtrl     <- newTVar $ agentAct behaviour
                    actUpdCtrl  <- newEmptyTMVar
                    msgCtrl     <- newTVar $ handleMessages behaviour
                    runState    <- newTVar Paused
                    runStateUpd <- newEmptyTMVar
                    messageBoxPriority  <- newTQueue
                    messageBox          <- newTQueue

                    return AgentRun {
                                 _agentId       = id,
                                 _agentDebug    = debug,
                                 _states        = states,
                                 _actCtrl       = actCtrl,
                                 _actUpdateCtrl = actUpdCtrl,
                                 _msgCtrl       = msgCtrl,
                                 _runState      = runState,
                                 _runStateUpdate        = runStateUpd,
                                 _messageBox            = messageBox,
                                 _messageBoxPriority    = messageBoxPriority
                               }
            let run'  = AgentRunOfRole run

            msgThreadStopped <- newEmptyMVar
            actThreadStopped <- newEmptyMVar

            -- Start threads
            msgThreadId  <- forkFinally  (forever $ _runAgentMessages run')
                                         (\_ -> do msgThreadStopped `putMVar` undefined
                                                   debugMsg run  "Message Thread Terminated"
                                                )
            actThreadId  <- forkFinally  (forever $ _runAgent run')
                                         (\_ -> do actThreadStopped `putMVar` undefined
                                                   debugMsg run "Act Thread Terminated"
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







