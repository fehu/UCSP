-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.Negotiator.Opinion
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module AUCSP.Agent.Negotiator.Opinion where


-----------------------------------------------------------------------------

import AUCSP.Agent.NegotiatingAgent

import Data.Set (Set)


-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- * Messages

data OpinionRequestType = OpinionRequestIndividual | OpinionRequestDeep

-- | Request _internal coherence_ assessment of `opinionAssume` by the agent
--   asked, in `opinionMode`.
data OpinionRequest (t :: OpinionRequestType) = OpinionRequest {
    opinionMode             :: ContextMode
  , opinionCandidateInfo    :: Set Class
  , opinionCandidateCreator :: CandidateCreator
  } deriving Typeable

setRequestType :: OpinionRequest t0 -> Proxy (t :: OpinionRequestType)
                                    -> OpinionRequest t
setRequestType (OpinionRequest m i c) _ = OpinionRequest m i c

data OpinionResponse = OpinionResponse {
    opinionEstimatedCoherence :: SomeCoherence
  , opinionEstimationMode     :: ContextMode
  }
  deriving (Typeable, Show)

type OpinionResponseConnections = Set AgentRef'

type instance ExpectedResponse (OpinionRequest OpinionRequestIndividual) =
              OpinionResponse
type instance ExpectedResponse (OpinionRequest OpinionRequestDeep) =
              (OpinionResponse, OpinionResponseConnections)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- * Handling Opinion Messages

-- handleOpinion :: (AgentInnerInterface i (RoleState r) (RoleResult r)) =>
--               r -> i -> OpinionRequest -> Respond OpinionResponse


class HandleOpinion (t :: OpinionRequestType) where
  handleOpinion :: ( CoherenceConstraints, Fractional Coherence
                   , AgentInnerInterface i (AgentState r) ()
                    ) =>
                   i
                -> OpinionRequest t
                -> IO (MsgResponse (ExpectedResponse (OpinionRequest t)))


instance HandleOpinion OpinionRequestIndividual where
  handleOpinion i req = do
        assessed <- propagateThroughContexts (contexts $ agentState i)
                                             (opinionMode req)
                  $ newCandidate (asInformation $ opinionCandidateInfo req)
                                 (opinionCandidateCreator req)
        respond $ OpinionResponse (someCoherence' $ candidateCoherence assessed)
                                  (opinionMode req)

instance HandleOpinion OpinionRequestDeep where
  handleOpinion i req = do
    coherence <- handleOpinion i
               $ setRequestType req (Proxy :: Proxy OpinionRequestIndividual)
    currentConnections <- undefined :: IO OpinionResponseConnections
    return $ flip (,) currentConnections <$> coherence

-- class HandleOpinion (t :: OpinionRequestType) where
--   handleOpinion :: (CoherenceConstraints, Fractional Coherence) =>
--                    Contexts Coherence
--                 -> OpinionRequest t
--                 -> IO (MsgResponse (ExpectedResponse (OpinionRequest t)))
--
-- instance HandleOpinion OpinionRequestIndividual where
--   handleOpinion cxts req = do
--         assessed <- propagateThroughContexts cxts (opinionMode req)
--                   $ newCandidate (asInformation $ opinionCandidateInfo req)
--                                  (opinionCandidateCreator req)
--         respond $ OpinionResponse (someCoherence' $ candidateCoherence assessed)
--                                   (opinionMode req)
--
-- instance HandleOpinion OpinionRequestDeep where
--   handleOpinion cxts req = do
--     coherence <- handleOpinion cxts
--                $ setRequestType req (Proxy :: Proxy OpinionRequestIndividual)
--     currentConnections <- undefined :: IO OpinionResponseConnections
--     return $ flip (,) currentConnections <$> coherence

-----------------------------------------------------------------------------
