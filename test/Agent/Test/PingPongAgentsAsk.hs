-----------------------------------------------------------------------------
--
-- Module      :  GenericAgent.Test.PingPongAgentsAsk
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE TypeFamilies #-}

module Agent.Test.PingPongAgentsAsk where


import Agent
-- import Agent.AgentImpl (whenM)
import Agent.Test.PingPong


import Data.Typeable(cast)
import Data.IORef


-----------------------------------------------------------------------------

type instance ExpectedResponse Ping = Pong

-----------------------------------------------------------------------------

askPing maxCount i state = do c <- readIORef $ count state
                              counterpart <- pingCounterpart state
                              if c < maxCount
                                 then do count state `writeIORef` (c+1)
                                         putStrLn "Ping!"
                                         Pong <- counterpart `ask` Ping
                                         askPing maxCount i state
                                 else do putStrLn "Finished"
                                         selfStop i

-- | TODO
pingBehaviour maxCount = AgentBehavior{
    handleMessages = AgentHandleMessages {
            handleMessage       = \_ _ -> undefined
          , respondMessage      = \_ _ -> undefined
          }
  , act = askPing maxCount
  }


-----------------------------------------------------------------------------

-- | Pong agent always responds 'Ping' messages with 'Pong'.
pongBehaviour = AgentBehavior{
  handleMessages = AgentHandleMessages{
          handleMessage  = \_ _ -> return undefined
        , respondMessage = \_ _ -> selectResponse [ mbResp $ \Ping -> return Pong ]
        }
  , act = \_ _ -> return ()
  }

-----------------------------------------------------------------------------

createPingPong = createPingPong' pingBehaviour pongBehaviour

testPingPong = testPingPong' pingBehaviour pongBehaviour

