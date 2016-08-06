-----------------------------------------------------------------------------
--
-- Module      :  GenericAgent.Test.PingPongAgentsAskT
-- License     :  MIT
--
-- |
--

{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}

module GenericAgent.Test.PingPongAgentsAsk2 where

import GenericAgent
import GenericAgent.Test.PingPong hiding (Ping, Pong)

import Data.Typeable (Typeable)

-----------------------------------------------------------------------------

newtype Ping = Ping Int deriving (Typeable, Show)
newtype Pong = Pong Int deriving (Typeable, Show)

type instance ExpectedResponse Ping = Pong

-----------------------------------------------------------------------------

askPing maxCount c i state = do counterpart <- pingCounterpart state
                                putStrLn $ "c = " ++ show c
                                if c < maxCount
                                 then do putStrLn "Ping!"
                                         Pong c' <- counterpart `ask` Ping c
                                         askPing maxCount c' i state
                                 else do putStrLn "Finished"
                                         selfStop i

-- | TODO
pingBehaviour :: Int -> AgentBehavior PingAgentState
pingBehaviour maxCount = AgentBehavior{
    handleMessages = AgentHandleMessages {
            handleMessage       = \_ _ -> undefined
          , respondMessage      = \_ _ -> undefined
          }
  , act = askPing maxCount 0
  }


-----------------------------------------------------------------------------

-- | TODO
pongBehaviour = AgentBehavior{
  handleMessages = AgentHandleMessages{
          handleMessage  = \_ _ -> undefined
        , respondMessage = \_ _ -> selectResponse [
              mbResp $ \(Ping c) -> return $ Pong (c + 1)
            ]
        }
  , act = \_ _ -> return ()
  }


-----------------------------------------------------------------------------

createPingPong = createPingPong' pingBehaviour pongBehaviour

testPingPong = testPingPong' pingBehaviour pongBehaviour

