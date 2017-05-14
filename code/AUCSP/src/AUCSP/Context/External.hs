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
{-# LANGUAGE ExistentialQuantification #-}

module AUCSP.Context.External (

  ExternalContext, ExternalContextConfig(..)
, newExternalContext, opinionRelation

, OpinionEstimation(..), OpinionDetails(..)
, SomeCoherence, someCoherence, someCoherence', getSomeCoherence

, module Export

) where

import CSP.Coherence.Candidate
import AUCSP.Context
import AUCSP.Classes
import AUCSP.Utils.InInterval as Export
import AUCSP.AgentsInterface.KnownAgents (SomeAgent, KnownAgentsConstraints)

import Data.Typeable (Typeable, cast)
import Data.Maybe (fromJust)
import qualified Data.List as List

import Control.Arrow ( (&&&) )

import Data.Set (Set)
import Data.Map (Map)

import qualified Data.Set as Set
import qualified Data.Map as Map


-----------------------------------------------------------------------------

type ExternalContext a = CombiningContext ContextMode IO IO
                                          DetailsByRel a
                                        [InUnitInterval a]

data ExternalContextConfig a = ExternalContextConfig{
    externalThreshold  :: IO a
  , externalAskOpinion :: Set Class -> SomeAgent -> IO OpinionEstimation
  , externalIsMyClass           :: Class -> Bool
  , externalGetClassCounterpart :: Class -> SomeAgent
  , externalCommonGoal :: [InUnitInterval a] -> InUnitInterval a
  }



data OpinionEstimation = OpinionEstimation { opinionEstimatedBy :: SomeAgent
                                           , opinionEstimation  :: SomeCoherence
                                           }

-----------------------------------------------------------------------------

data SomeCoherence = forall a . (Fractional a, Show a, Ord a, Typeable a) =>
    SomeCoherence (InUnitInterval a)
instance Show SomeCoherence where show (SomeCoherence x) = show x

someCoherence :: (Fractional a, Show a, Ord a, Typeable a) =>
                 a -> Maybe SomeCoherence
someCoherence = fmap SomeCoherence . inInterval

someCoherence' :: (Fractional a, Show a, Ord a, Typeable a) =>
                 a -> SomeCoherence
someCoherence' = fromJust . someCoherence

getSomeCoherence :: (Fractional a, Ord a, Typeable a) =>
                    SomeCoherence -> InUnitInterval a
getSomeCoherence (SomeCoherence c) = fromJust $ rmap (fromJust . cast) c

-----------------------------------------------------------------------------

newExternalContext :: ( Typeable a, Show a, Fractional a, Ord a
                      , KnownAgentsConstraints
                        ) =>
                      ExternalContextConfig a -> ExternalContext a
newExternalContext cfg =
  let filtering = newIOFilteringContext "External"
                  (return Set.empty)
                  (newCtxRels [SomeContextRelation $ opinionRelation cfg] head)
                  (return $ Threshold []) -- TODO  (externalThreshold cfg)
      combineCtxCoh internal' externals =
        fromInterval . externalCommonGoal cfg
                     $ toInterval internal' : externals
  in CombiningContext filtering combineCtxCoh (externalThreshold cfg)

-----------------------------------------------------------------------------

-- | Opinion relation estimates external coherence of depth 0 in Preliminary mode.
-- 1. Select candidate's classes that mention the agent.
-- 2. Find the 'counterparts' of the classes.
-- 3. Ask each counterpart its Opinion and wait responses.
opinionRelation :: ( Typeable a, Show a, Fractional a, Ord a
                   , KnownAgentsConstraints
                    ) =>
               ExternalContextConfig a
            -> CtxWholeRelation ContextMode IO OpinionDetails [InUnitInterval a]
opinionRelation cfg = CtxWholeRelation "Opinion" $
  \mode inf -> Just $ do -- Maybe (IO ([InUnitInterval a], OpinionDetails))
        let myClasses = filter (externalIsMyClass cfg) $ collectInformation id inf
            myClasses' = Set.fromList myClasses
            counterparts = List.nub $ map (externalGetClassCounterpart cfg)
                                          myClasses
        opinions <- mapM (externalAskOpinion cfg myClasses') counterparts
        let mkDetail = opinionEstimatedBy &&& opinionEstimation
            details  = OpinionDetails . Map.fromList $ map mkDetail opinions
            coherences = map (getSomeCoherence . opinionEstimation) opinions
        return (coherences, details)

newtype OpinionDetails = OpinionDetails (Map SomeAgent SomeCoherence)


-----------------------------------------------------------------------------
