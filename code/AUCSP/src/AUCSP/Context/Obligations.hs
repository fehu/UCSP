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

, minimumClasses, MinimumClassesRelDetails(..)
, maximumClasses, MaximumClassesRelDetails(..)

) where

import AUCSP.Context
import AUCSP.Classes
import AUCSP.AgentsInterface.KnownAgents

import qualified Data.Set as Set

-----------------------------------------------------------------------------

type ObligationsContext = FilteringContextPure ContextMode
                                               DetailsByRel
                                               Bool

-- | Boolean relations are combined using `and`.
newObligationsContext :: Information -> SomeRelations Id Bool
                                     -> ObligationsContext
newObligationsContext cdata rels = newPureFilteringContext "Obligations"
                                    cdata (newCtxRels rels and) (Threshold True)

instance (Num a, Ord a) => CtxRelationValueConvertable Bool a where
  ctxConvertRelationValue b = if b then 1 else 0
  ctxConvertRelationValue'  = (>= 1)

-----------------------------------------------------------------------------
-- * Some Possible Relations

-- | Must have at least X minutes of classes (total).
--   Supports 'ContextModePreliminary'.+
minimumClasses :: (KnownAgentsConstraints) => Minutes
               -> CtxWholeRelation ContextMode Id MinimumClassesRelDetails Bool
minimumClasses minc = CtxWholeRelation "MinimumClasses" $
         \_ inf ->
            let cLength = myClassesDuration inf
                details = if cLength >= minc then EnoughClasses
                                             else NotEnoughClasses cLength minc
            in Just $ return (details == EnoughClasses, details)

data MinimumClassesRelDetails = EnoughClasses
                              | NotEnoughClasses Minutes Minutes
  deriving (Show, Eq, Ord)

myClassesDuration :: (KnownAgentsConstraints) => Information -> Minutes
myClassesDuration inf = let [me] = collectInformation getSelfAgent inf
                         in sum . map classDuration
                                . filter (classRefersAgent me)
                                $ collectInformation id inf


-----------------------------------------------------------------------------

-- | Must have maximum X minutes of classes (total). Ignores mode.
maximumClasses :: (KnownAgentsConstraints) => Minutes
               -> CtxWholeRelation ContextMode Id MaximumClassesRelDetails Bool
maximumClasses maxc = CtxWholeRelation "MaximumClasses" $
         \_ inf ->
            let cLength = myClassesDuration inf
                details = if cLength <= maxc then NotTooMuchClasses
                                             else TooMuchClasses cLength maxc
            in Just $ return (details == NotTooMuchClasses, details)


data MaximumClassesRelDetails = NotTooMuchClasses
                              | TooMuchClasses Minutes Minutes
  deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------
