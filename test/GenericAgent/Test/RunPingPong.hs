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

import qualified GenericAgent.Test.PingPongAgentsSend as PPASend
import qualified GenericAgent.Test.PingPongAgentsAsk  as PPAAsk

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




