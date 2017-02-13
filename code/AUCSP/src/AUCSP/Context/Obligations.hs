-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Context.Obligations
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AUCSP.Context.Obligations (

  ObligationsContext, newObligationsContext

-- * Some Possible Relations

, minimumClasses, MinimumClassesRel, MinimumClassesRelDetails(..)
, maximumClasses, MaximumClassesRel, MaximumClassesRelDetails(..)

) where

import CSP.Coherence.Context.Filtering
import AUCSP.Context
import AUCSP.Classes
import AUCSP.AgentsInterface.KnownAgents

import qualified Data.Set as Set

-----------------------------------------------------------------------------

type ObligationsContext = FilteringContextPure ContextMode
                                               DetailsByRel
                                               Bool

newObligationsContext :: Information -> Relations Id Bool -> ObligationsContext
newObligationsContext cdata rels = newPureFilteringContext "Obligations"
                                    cdata (CtxRelations rels) (Threshold True)

instance (Num a, Ord a) => CtxRelationValueConvertable Bool a where
  ctxConvertRelationValue b = if b then 1 else 0
  ctxConvertRelationValue'  = (>= 1)

-----------------------------------------------------------------------------
-- * Some Possible Relations

-- | Must have at least X minutes of classes (total).
--   Supports 'ContextModePreliminary'.
minimumClasses :: Minutes -> MinimumClassesRel
minimumClasses = MinimumClassesRel


data MinimumClassesRel = MinimumClassesRel Minutes
data MinimumClassesRelDetails = EnoughClasses
                              | NotEnoughClasses Minutes Minutes
  deriving (Show, Eq, Ord)

instance (KnownAgentsConstraints) =>
  ContextRelation MinimumClassesRel ContextMode Id Bool where
    type RelationDetails MinimumClassesRel = MinimumClassesRelDetails
    relationName _ = "MinimumClasses"
    assessRelation _ ContextModePreliminary _ = Just $ return (True, EnoughClasses)
    assessRelation (MinimumClassesRel minc) _ inf =
      let cLength = myClassesDuration inf
          details = if cLength >= minc then EnoughClasses
                                       else NotEnoughClasses cLength minc
      in Just $ return (details == EnoughClasses, details)


myClassesDuration :: (KnownAgentsConstraints) => Information -> Minutes
myClassesDuration inf = let [me] = collectInformation getSelfAgent inf
                         in sum . map classDuration
                                . filter (classRefersAgent me)
                                $ collectInformation id inf


-----------------------------------------------------------------------------

-- | Must have maximum X minutes of classes (total). Ignores mode.
maximumClasses :: Minutes -> MinimumClassesRel
maximumClasses = undefined


data MaximumClassesRel = MaximumClassesRel Minutes
data MaximumClassesRelDetails = NotTooMuchClasses
                              | TooMuchClasses Minutes Minutes
  deriving (Show, Eq, Ord)

instance (KnownAgentsConstraints) =>
  ContextRelation MaximumClassesRel ContextMode Id Bool where
    type RelationDetails MaximumClassesRel = MaximumClassesRelDetails
    relationName _ = "MaximumClasses"
    assessRelation (MaximumClassesRel maxc) _ inf =
      let cLength = myClassesDuration inf
          details = if cLength <= maxc then NotTooMuchClasses
                                       else TooMuchClasses cLength maxc
      in Just $ return (details == NotTooMuchClasses, details)

-----------------------------------------------------------------------------
