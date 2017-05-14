-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Context
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AUCSP.Context(

  ContextMode(..)

, SelfAgent(..), getSelfAgent

, DetailsByRel, detailsByRel, findDetails

, Relations(..), SomeRelations, newCtxRels

, module Export

) where

import CSP.Coherence.Context.Filtering as Export
import AUCSP.AgentsInterface

import Data.Typeable (Typeable)

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Arrow (first, second)

-----------------------------------------------------------------------------

data ContextMode = ContextModePreliminary | ContextModeFinal

instance Show ContextMode where
  show ContextModePreliminary = "Preliminary"
  show ContextModeFinal       = "Final"

-----------------------------------------------------------------------------

newtype SelfAgent = SelfAgent SomeAgent
  deriving (Typeable, Show, Eq, Ord)

getSelfAgent (SelfAgent a) = a

instance InformationPiece SelfAgent where
  informationType _ = "Agent self reference"

-----------------------------------------------------------------------------

newtype DetailsByRel = DetailsByRel (Set DetailsOfRel)
detailsByRel (DetailsByRel rels) = detailsOfRel <$> Set.elems rels

findDetails :: (Functor m) => SomeContextRelation mode m a -> DetailsByRel
            -> Maybe SomeRelationDetails
findDetails rel (DetailsByRel rels) = detailsOfRel <$> findInSet rel rels
  where findInSet  k = findInSet' . FindDetails $ AnyContextRelation rel
        findInSet' k = filterMaybe (k ==) . Set.lookupLE k
        filterMaybe f mb = do b <- mb
                              if f b then Just b else Nothing

newDetailsByRel = DetailsByRel . Set.fromList . map DetailsOfRel

data AnyContextRelation = forall mode m a . Functor m =>
     AnyContextRelation (SomeContextRelation mode m a)

data DetailsOfRel = DetailsOfRel SomeRelationDetails
                  | FindDetails  AnyContextRelation
detailsOfRel (DetailsOfRel rel) = rel
detailsRelationName (DetailsOfRel (SomeRelationDetails rel _)) = relationName rel

onRelationName f (DetailsOfRel (SomeRelationDetails r1 _))
                 (DetailsOfRel (SomeRelationDetails r2 _)) =
       relationName r1 `f` relationName r2

onRelationName f (FindDetails  (AnyContextRelation r1))
                 (DetailsOfRel (SomeRelationDetails r2 _)) =
       relationName r1 `f` relationName r2
onRelationName f x@(DetailsOfRel _) y@(FindDetails _) = onRelationName f y x


instance Eq   DetailsOfRel where (==) = onRelationName (==)
instance Ord  DetailsOfRel where compare = onRelationName compare


instance Show DetailsByRel where
  show (DetailsByRel rels) = "Details for relations: " ++
                             show (map detailsRelationName $ Set.elems rels)

-----------------------------------------------------------------------------

type SomeRelations m a = [SomeContextRelation ContextMode m a]

data Relations m a = Relations {
    getRelations     :: SomeRelations m a
  , combineRelations :: [a] -> a
}

newCtxRels rels = CtxRelations . Relations rels

instance CtxRelations' (Relations m a) ContextMode m DetailsByRel a where
  ctxRelations' = getRelations
  ctxCombineRels rels = let comb = combineRelations rels
                        in first comb . second newDetailsByRel . unzip


-----------------------------------------------------------------------------
