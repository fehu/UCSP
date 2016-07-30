-----------------------------------------------------------------------------
--
-- Module      :  GenericAgent.Test.PingPongAgentsAskT
-- License     :  MIT
--
-- |
--

{-# LANGUAGE TypeFamilies, FlexibleContexts, ScopedTypeVariables #-}

module GenericAgent.Test.PingPongAgentsAskT where

import GenericAgent
import GenericAgent.AgentImpl (whenM)
import GenericAgent.Test.PingPong hiding (Ping, Pong)


import Data.Typeable (Typeable, Typeable1, cast)
import Data.IORef


-----------------------------------------------------------------------------

newtype Ping a = Ping a deriving (Typeable, Show)
newtype Pong a = Pong a deriving (Typeable, Show)

type instance ExpectedResponse1 Ping = Pong

-----------------------------------------------------------------------------

askPing maxCount c i state = do counterpart <- pingCounterpart state
                                putStrLn $ "c = " ++ show c
                                if c < maxCount
                                 then do putStrLn "Ping!"
                                         Pong c' <- counterpart `askT` Ping c
                                         askPing maxCount c' i state
                                 else do putStrLn "Finished"
                                         selfStop i

-- | TODO
pingBehaviour :: Int -> AgentBehavior PingAgentState
pingBehaviour maxCount = AgentBehavior{
    handleMessages = AgentHandleMessages {
            handleMessage       = \_ _ -> undefined
          , respondMessage      = \_ _ -> undefined
          , respondTypedMessage = \_ _ -> undefined
          }
  , act = askPing maxCount 0
  }


-----------------------------------------------------------------------------

-- | TODO
pongBehaviour = AgentBehavior{
  handleMessages = AgentHandleMessages{
          handleMessage  = \_ _ -> undefined
        , respondMessage = \_ _ -> undefined
        , respondTypedMessage = \_ _ msg ->
            case cast' msg of Just (Ping c) ->
                                case cast c of Just (x :: Int) -> do putStrLn "Pong!"
                                                                     let pong = Pong (x + 1)
                                                                         Just r = cast pong
                                                                     return r -- TODO
        }
  , act = \_ _ -> return ()
  }


cast' :: (Typeable a, Typeable b, Typeable x) => a x -> Maybe (b x)
cast' = cast

-----------------------------------------------------------------------------

createPingPong = createPingPong' pingBehaviour pongBehaviour

testPingPong = testPingPong' pingBehaviour pongBehaviour


