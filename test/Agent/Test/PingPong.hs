-----------------------------------------------------------------------------
--
-- Module      :  GenericAgent.Test.PingPong
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Agent.Test.PingPong where

import Agent
import Agent.Extra
import Agent.Manager

import Data.IORef
import Data.Typeable

-----------------------------------------------------------------------------

data Ping = Ping deriving (Show, Typeable)
data Pong = Pong deriving (Show, Typeable)

-----------------------------------------------------------------------------


data PingAgentState = PingAgentState { pingCounterpart  :: IO AgentRef
                                     , count            :: IORef Int
                                     , firstTime        :: IORef Bool
                                     }



data PongAgentState = PongAgentState { pongCounterpart :: IO AgentRef }


-----------------------------------------------------------------------------

pingDescriptor pingBehaviour counterpart maxCount = AgentDescriptor{
    agentBehaviour = pingBehaviour maxCount
  , newAgentStates = do count <- newIORef 0
                        first <- newIORef True
                        return $ PingAgentState counterpart count first
  , nextAgentId    = const . return $ AgentId "Ping"
  }


pongDescriptor pongBehaviour counterpart = AgentDescriptor pongBehaviour
                                             (return $ PongAgentState counterpart)
                                             (const . return $ AgentId "Pong")

-----------------------------------------------------------------------------

createPingPong' pingBehaviour pongBehaviour maxCount =
    do pingRef <- newIORef undefined
       pongRef <- newIORef undefined

       let pingD = pingDescriptor pingBehaviour (readIORef pongRef) maxCount
           pongD = pongDescriptor pongBehaviour (readIORef pingRef)

       m  <- newAgentsManager :: IO SimpleAgentsManager

       (ping :: GenericAgent, piFRref) <- m `createWithManager` pingD
       (pong :: GenericAgent, poFRref) <- m `createWithManager` pongD

       pingRef `writeIORef` fromFullRef piFRref
       pongRef `writeIORef` fromFullRef poFRref

       return (m, (piFRref, poFRref))


testPingPong' pingBehaviour pongBehaviour maxCount =
    do (m, (ping, pong)) <- createPingPong' pingBehaviour pongBehaviour maxCount
       ping `sendPriority` StartMessage
       pong `sendPriority` StartMessage

       putStrLn "Waiting for Ping"
       waitAgent ping

       putStrLn "Stopping"
       m `orderEachAgent` StopMessage

       return "Done"

