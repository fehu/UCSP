-----------------------------------------------------------------------------
-- GenericAgent.Test
-----------------------------------------------------------------------------

module GenericAgent.Test ( main ) where

import GenericAgent.Test.PingPongAgents (testPingPong)

import Test.Hspec

main = hspec $
    describe "PingPongAgents" $
        they "shoud be able to communicate by sending messages"
            $ testPingPong 5 `shouldReturn` "Done"





they = it
