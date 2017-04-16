-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Classes.CoresGeneration
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module AUCSP.Classes.CoresGeneration (

  GroupClassCore(..), groupCoreClasses
, GroupClassCorePool, nextClassCore

, generateClassCores

) where

import AUCSP.AgentsInterface.RoleData
import AUCSP.Utils.Random
-- import AUCSP.Classes
import AUCSP.Utils.List

import qualified Data.Set as Set
import Data.Set (member)

-----------------------------------------------------------------------------

newtype GroupClassCore = GroupClassCore [(Discipline, KnownAgent Professor)]

groupCoreClasses :: RoleData Group -> GroupClassCore -> [ClassCore]
groupCoreClasses rdata (GroupClassCore dps) =
  let g = knownSelf rdata   in   map (\(d,p) -> ClassCore d g p) dps

newtype GroupClassCorePool = GroupClassCorePool [GroupClassCore]


nextClassCore :: (KnownAgentsConstraints) =>
                       GroupClassCorePool
                   -> Maybe (GroupClassCore, GroupClassCorePool)
nextClassCore (GroupClassCorePool (c:cs)) = Just (c, GroupClassCorePool cs)
nextClassCore _                           = Nothing

generateClassCores :: (KnownAgentsConstraints) =>
                      RoleData Group -> IO GroupClassCorePool
generateClassCores rdata = do
  let ds    = Set.toList . groupNeedsDisciplines $ capabilities rdata
      canTeach d p = d `member` professorCanTeach (capabilities $ knownData p)
  profs <- knownProfessors $ knownAgents rdata
  -- for each discipline needed
  -- select professors, that can teach it.
  -- Randomize professors lists.
  dps <- sequence $ do  d <- ds
                        let ps = filter (canTeach d) profs
                        return $ (,) d <$> randomList ps
  -- combine suiting professors with disciplines.
  return . GroupClassCorePool . map GroupClassCore $ groupCombinations dps

-----------------------------------------------------------------------------
