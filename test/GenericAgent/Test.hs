-----------------------------------------------------------------------------
-- GenericAgent.Test
-----------------------------------------------------------------------------

module Main ( main ) where

import qualified GenericAgent.Test.PingPongAgentsSend as PPASend
import qualified GenericAgent.Test.PingPongAgentsAsk  as PPAAsk
import qualified GenericAgent.Test.PingPongAgentsAskT as PPAAskT

import Test.Hspec


maxCount = 10

main = hspec $
    describe "## PingPongAgents" $ do
        they "# shoud be able to communicate by sending messages"
            $ PPASend.testPingPong maxCount `shouldReturn` "Done"

        they "# shoud be able to communicate by asking"
            $ PPAAsk.testPingPong maxCount `shouldReturn` "Done"

        they "# shoud be able to communicate by asking (typed)"
            $ PPAAskT.testPingPong maxCount `shouldReturn` "Done"


they = it
