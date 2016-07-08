-----------------------------------------------------------------------------
--
-- Module      :  GenericAgent.Test.PingPongAgents
-- License     :  MIT
--
-- | Test of agents messaging (`send` function) with simple Ping and Pong messages.
--

{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

module GenericAgent.Test.PingPongAgents where

import GenericAgent
import GenericAgent.AgentImpl
import Extra

import Data.Typeable
import Data.IORef

import Control.Monad

-----------------------------------------------------------------------------

data Ping = Ping deriving (Show, Typeable)
data Pong = Pong deriving (Show, Typeable)

-----------------------------------------------------------------------------

data PingAgentState = PingAgentState { pingCounterpart  :: IO AgentRef
                                     , count            :: IORef Int
                                     , firstTime        :: IORef Bool
                                     }

-- | Ping agent sends 'Ping' message to it's counterpart at start.
--   Responds to the first `maxCount` 'Pong' messages with 'Ping'.
pingBehaviour maxCount = AgentBehaviour{
    handleMessages = AgentHandleMessages {
            handleMessage = \i state msg ->
                case cast msg of Just Pong -> do c <- readIORef $ count state
                                                 counterpart <- pingCounterpart state
                                                 if c < maxCount
                                                    then do count state `writeIORef` (c+1)
                                                            putStrLn "Ping!"
                                                            counterpart `send` Ping
                                                    else do putStrLn "Finished"
                                                            selfStop i
                                 _         -> return ()
          , respondMessage      = \_ _ -> return undefined
          , respondTypedMessage = \_ _ -> return undefined
          }
  , act = \i state -> whenM (readIORef $ firstTime state)
                          $ do firstTime state `writeIORef` False
                               putStrLn "Ping!"
                               pingCounterpart state >>= (`send` Ping)
  }

pingDescriptor counterpart maxCount = AgentDescriptor{
    agentBehaviour = pingBehaviour maxCount
  , newAgentStates = do count <- newIORef 0
                        first <- newIORef True
                        return $ PingAgentState counterpart count first
  , nextAgentId    = return $ AgentId "Ping"
  }

-----------------------------------------------------------------------------

data PongAgentState = PongAgentState { pongCounterpart :: IO AgentRef }

-- | Pong agent always responds 'Ping' messages with 'Pong'.
pongBehaviour = AgentBehaviour{
  handleMessages = AgentHandleMessages{
          handleMessage = \i state msg ->
            case cast msg of Just Ping -> do putStrLn "Pong!"
                                             pongCounterpart state >>= (`send` Pong)
                             _         -> return ()
        , respondMessage      = \_ _ -> return undefined
        , respondTypedMessage = \_ _ -> return undefined
        }
  , act = \_ _ -> return ()
  }

pongDescriptor counterpart = AgentDescriptor pongBehaviour
                                             (return $ PongAgentState counterpart)
                                             (return $ AgentId "Pong")

-----------------------------------------------------------------------------

createPingPong maxCount = do pingRef <- newIORef undefined
                             pongRef <- newIORef undefined

                             let pingD = pingDescriptor (readIORef pongRef) maxCount
                                 pongD = pongDescriptor (readIORef pingRef)

                             m  <- newAgentsManager :: IO SimpleAgentsManager

                             (ping :: GenericAgent, piFRref) <- m `createWithManager` pingD
                             (pong :: GenericAgent, poFRref) <- m `createWithManager` pongD

                             pingRef `writeIORef` fromFullRef piFRref
                             pongRef `writeIORef` fromFullRef poFRref

                             return (m, (piFRref, poFRref))


testPingPong maxCount = do (m, (ping, pong)) <- createPingPong maxCount
                           ping `sendPriority` StartMessage
                           pong `sendPriority` StartMessage

                           putStrLn "Waiting for Ping"
                           waitAgent ping

                           putStrLn "Stopping"
                           m `orderEachAgent` StopMessage

                           return "Done"







