-----------------------------------------------------------------------------
--
-- Module      :  GenericAgent.Test.PingPongAgents
-- License     :  MIT
--
-- | Test of agents messaging (`send` function) with simple Ping and Pong messages.
--

--{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

module GenericAgent.Test.PingPongAgentsSend where

import GenericAgent
import GenericAgent.AgentImpl (whenM)
import GenericAgent.Test.PingPong

import Data.Typeable(cast)
import Data.IORef

import Control.Monad

-----------------------------------------------------------------------------

-- | Ping agent sends 'Ping' message to it's counterpart at start.
--   Responds to the first `maxCount` 'Pong' messages with 'Ping'.
pingBehaviour maxCount = AgentBehavior{
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

-----------------------------------------------------------------------------

-- | Pong agent always responds 'Ping' messages with 'Pong'.
pongBehaviour = AgentBehavior{
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

-----------------------------------------------------------------------------

createPingPong = createPingPong' pingBehaviour pongBehaviour

testPingPong = testPingPong' pingBehaviour pongBehaviour






