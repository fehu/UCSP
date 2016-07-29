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

module GenericAgent.Test.PingPongAgentsAsk where


import GenericAgent
import GenericAgent.AgentImpl (whenM)
import GenericAgent.Test.PingPong


import Data.Typeable(cast)
import Data.IORef


-----------------------------------------------------------------------------

type instance ExpectedResponse Ping = Pong
--type instance ExpectedResponse Pong = Maybe Ping

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

--askPing i state = whenM (readIORef $ firstTime state)
--                  $ do firstTime state `writeIORef` False
--                       putStrLn "Ping!"
--                       Pong <- pingCounterpart state >>= (`ask` Ping)
--                       askPing i state

-- | Ping agent sends 'Ping' message to it's counterpart at start.
--   Responds to the first `maxCount` 'Pong' messages with 'Ping'.
pingBehaviour maxCount = AgentBehavior{
    handleMessages = AgentHandleMessages {
            handleMessage       = \_ _ -> undefined
          , respondMessage      = \_ _ -> undefined
          , respondTypedMessage = \_ _ -> undefined
          }
  , act = askPing maxCount
  }


-----------------------------------------------------------------------------

-- | Pong agent always responds 'Ping' messages with 'Pong'.
pongBehaviour = AgentBehavior{
  handleMessages = AgentHandleMessages{
          handleMessage  = \_ _ -> return undefined
        , respondMessage = \_ _ msg ->
            case cast msg of Just Ping -> do putStrLn "Pong!"
                                             let Just r = cast Pong
                                             return r -- TODO
        , respondTypedMessage = \_ _ -> return undefined
        }
  , act = \_ _ -> return ()
  }

-----------------------------------------------------------------------------

createPingPong = createPingPong' pingBehaviour pongBehaviour

testPingPong = testPingPong' pingBehaviour pongBehaviour

