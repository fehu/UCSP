-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Context.External
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}

module AUCSP.Context.External (

  ExternalContext, newExternalContext

, AskOpinion, opinionRelation
, OpinionDetails(..), OpinionResponse(..), OpinionBestCoherence(..)
, SomeCoherence(..), getSomeCoherence

, module Export

) where

import AUCSP.Context
import AUCSP.Classes
import AUCSP.Utils.InInterval            as Export
import AUCSP.AgentsInterface.KnownAgents as Export

import Data.Typeable (Typeable, cast)
import Data.Maybe (fromJust)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad (forM)

-----------------------------------------------------------------------------

type ExternalContext a = FilteringContextIO ContextMode
                                            DetailsByRel
                                           (InUnitInterval a)
newExternalContext :: (Ord a, Fractional a, Show a, Typeable a, KnownAgentsConstraints) =>
                      AskOpinion
                   -> KnownAgents
                   -> SelfAgent
                   -> ExternalContext a
newExternalContext askOpinion known self =
  newIOFilteringContext "External"
      (Set.fromList <$> counterparts known)
      (newCtxRels [SomeContextRelation $ opinionRelation askOpinion] head)
      opinionThreshold
    where counterparts = case self
            of SelfAgent a
                | someAgentIsOfRole a Group     -> mkAgentInf . knownProfessors
                | someAgentIsOfRole a Professor -> mkAgentInf . knownGroups

          opinionRel   = undefined


type AskOpinion = SomeAgent -> OpinionRequest -> IO OpinionResponse

-----------------------------------------------------------------------------

mkAgentInf :: (AgentOfRole r) => IO [KnownAgent r] -> IO [SomeInformationPiece]
mkAgentInf = fmap (map (SomeInformationPiece . someAgent))

instance InformationPiece SomeAgent where
  informationType (SomeAgent r _) = "SomeAgent (" ++ show r ++ ")"


-----------------------------------------------------------------------------

-- | Asks opinion about given classes. Ignores mode.
--   Estimates coherence of a response as sum of:
--      1. 'opinionCoherenceInternal' scaled to [0, 1]
--      2. 'opinionCoherenceWithBest' scaled to [1, 2]
--      3. 10, if is coherent with the best.
--   Resulting coherence is a product of scaled ([0,13] -> [0,1]) opinion coherences.
-- Threshold should be 10/13.

opinionRelation :: (Fractional a, Ord a, Typeable a, KnownAgentsConstraints) =>
                   AskOpinion
                -> CtxWholeRelation ContextMode IO OpinionDetails
                                    (InUnitInterval a)
opinionRelation askOpinion = CtxWholeRelation "External" $
    \_ inf -> Just $ do
        let counterparts = collectInformation id inf
            classes      = collectInformation id inf
            question  = OpinionRequest $ Set.fromList classes
        responses <- forM counterparts (`askOpinion` question)
        let details = OpinionDetails $ Map.fromList $
                      counterparts `zip` responses
            respCohs = do resp <- responses
                          let cohInternal = getSomeCoherence
                                          $ opinionCoherenceInternal resp
                              cohExternal = getSomeCoherence
                                          $ opinionCoherenceWithBest resp
                              cohExternal' = scaleToInterval cohExternal
                                            (Proxy :: Proxy '(Pos 1, Pos 2))
                              cohWithBest = if opinionCoherentWithBest resp
                                            then 10 else 0
                              bigScale' :: (Fractional a, Ord a) =>
                                           a -> InAnInterval Zero (Pos 13) a
                              bigScale' = toInterval
                              bigScale = bigScale'
                                       $ if opinionCoherentInternal resp
                                          then fromInterval cohInternal +
                                               fromInterval cohExternal' +
                                               cohWithBest
                                          else 0
                          return $ scaleToInterval bigScale
                                   (Proxy :: Proxy '(Zero, Pos 1))
            coh = toInterval . product $ map fromInterval respCohs
        return (coh, details)

opinionThreshold :: (Fractional a, Ord a) => IO (Threshold (InUnitInterval a))
opinionThreshold = return . Threshold . toInterval $ 10/13

newtype OpinionDetails = OpinionDetails (Map SomeAgent OpinionResponse)


data OpinionRequest  = OpinionRequest (Set Class) deriving Typeable
deriving instance (KnownAgentsConstraints) => Show OpinionRequest

data SomeCoherence = forall a . (Fractional a, Show a, Ord a, Typeable a) =>
     SomeCoherence (InUnitInterval a)
instance Show SomeCoherence where show (SomeCoherence x) = show x

getSomeCoherence :: (Fractional a, Ord a, Typeable a) =>
                    SomeCoherence -> InUnitInterval a
getSomeCoherence (SomeCoherence c) = fromJust $ rmap (fromJust . cast) c

data OpinionResponse = OpinionResponse {
    opinionCoherentInternal  :: Bool
  , opinionCoherenceInternal :: SomeCoherence
  , opinionDetailsInternal   :: DetailsByRel
  , opinionCoherentWithBest  :: Bool
  , opinionCoherenceWithBest :: SomeCoherence
  , opinionBestContradiction :: OpinionBestCoherence
  }
  deriving Typeable
deriving instance (KnownAgentsConstraints) => Show OpinionResponse

data OpinionBestCoherence
  = OpinionBestCoherent { opinionRespBest :: Set Class }
  | OpinionBestContradiction{
      opinionRespBest           :: Set Class
    , opinionRespContradictions :: [((Class, Class), SomeRelationDetails)]
    }
  deriving Typeable
deriving instance (KnownAgentsConstraints) => Show OpinionBestCoherence

instance Show SomeRelationDetails where show _ = "<???>"

-----------------------------------------------------------------------------
