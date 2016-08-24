-----------------------------------------------------------------------------
--
-- Module      :  GenericAgent.Test.RunPingPong
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

module Main where

import qualified Agent.Test.PingPongAgentsSend as PPASend
import qualified Agent.Test.PingPongAgentsAsk  as PPAAsk
import qualified Agent.Test.PingPongAgentsAsk2 as PPAAsk2

import System.Environment

-----------------------------------------------------------------------------


defaultMaxCount = 10

main = do args <- getArgs
          let maxCount = case args of [s] -> read s
                                      _   -> defaultMaxCount

          putStrLn "Testing Ping-Pong (send)"
          PPASend.testPingPong maxCount

          putStrLn "Testing Ping-Pong (ask)"
          PPAAsk.testPingPong maxCount

          putStrLn "Testing Ping-Pong (ask 2)"
          PPAAsk2.testPingPong maxCount


