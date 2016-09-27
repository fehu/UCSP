-----------------------------------------------------------------------------
--
-- Module      :  Agent.Manager.CreateAgent
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

module Agent.Manager.CreateAgent (

  CreateAgent(..), CreateAgents(..), AgentsCreated(..)

, handleCreateAgents, responseCreateAgents


) where

import Agent.Abstract
import Agent.Manager

import Data.Typeable
import Data.Maybe (fromJust)

import Control.Monad

-- -----------------------------------------------

data CreateAgent states res ag s = CreateAgent{
    crAgDescriptor  :: AgentDescriptor states res,
    crAgExtState    :: ag -> s
    }

data CreateAgents = forall states res ag s . ( Typeable ag, Typeable s, Show s
                                             , AgentCreate (AgentDescriptor states res) ag ) =>
     CreateAgents [CreateAgent states res ag s]
    deriving Typeable

data AgentsCreated = forall s . (Typeable s, Show s) =>
     AgentsCreated [(AgentFullRef, s)]
    deriving Typeable

type instance ExpectedResponse CreateAgents = AgentsCreated

instance Show CreateAgents where
    show (CreateAgents fs) = "CreateAgents " ++ show (length fs)

instance Show AgentsCreated where
    show (AgentsCreated as)  = "AgentsCreated failed:" ++ show as


responseCreateAgents  :: ( resp ~ ExpectedResponse msg
                         , Message msg, Message resp
                         , AgentsManager m s
                         , Typeable s, Show s
                         )
                      => m -> msg -> Maybe (IO resp)
responseCreateAgents m = mbResp $ \(CreateAgents cas) -> createAgents_ cas m

handleCreateAgents  :: ( Message msg, AgentsManager m s
                       , Typeable s, Show s )
                    => m -> msg -> Maybe (IO ())
handleCreateAgents m = mbHandle $ \(CreateAgents cas) -> void $ createAgents_ cas m

createAgents_ cas manager =
        liftM AgentsCreated $
        sequence =<<
        forM cas (
          \(CreateAgent d sf) -> return $
            do (ag, ref) <- createAgent d
               let eState = fromJust . cast $ sf ag
               manager `registerAgent` (ref, eState)
               return (ref, eState)
               )


