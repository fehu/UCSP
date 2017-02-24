-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.NegotiatingAgent.SearchCandidates
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module AUCSP.Agent.NegotiatingAgent.SearchCandidates(

  searchCandidates, GroupExtraState(..)

) where

import AgentSystem.Generic

import CSP.Coherence
import CSP.Coherence.Context.Filtering.Convert

import AUCSP.Classes.Generation

import AUCSP.Agent.NegotiatingAgent.NegotiationDefinition
import AUCSP.Agent.NegotiatingAgent.Decide

import Data.Typeable (Typeable)
import Data.Maybe (fromMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

import Control.Concurrent.STM

-----------------------------------------------------------------------------

type instance StateExtra Group a = GroupExtraState a

data GroupExtraState a = GroupExtraState {
    lastDecision        :: TVar (Maybe (GroupDecision a))
  , classroomSet        :: Set Classroom
  , groupRoleData       :: RoleData' Group
  , groupDecider        :: GroupDecider a
  }

-- | Follow last decision.
--   If no decision exists, create ClassCorePool and act as if it was `UseNextClassCore`.
--   In case 'GenerateTimeRoom' and 'UseNextClassCore':
--      generate a candidate, assess it and pass to decider.
--   In case 'NegotiateOver': start negotiation over candidate.
searchCandidates :: (DiscreteTimeDescriptor td, Num a, Ord a, Show a, Typeable a) =>
                    td -> AgentAction (RoleState (RoleT Group a))
                                      (RoleResult (RoleT Group a))
searchCandidates td = AgentAction $
  \i -> do let s     = agentState i
               extra = stateExtra s
           decision <- readTVarIO $ lastDecision extra
           case decision
             of Just (NegotiateOver c)    -> startNegotiationOver s c

                Just (ConfirmCoherence c) -> groupAssessAndDecide s c
                                              ContextModeFinal

                Just (GenerateTimeRoom c) -> generateAssessAndDecide s c
                                              ContextModePreliminary
                                              usingSameClassCore

                Just (UseNextClassCore c) -> generateAssessAndDecide s c
                                              ContextModePreliminary
                                              usingNextClassCore

                Just UpdateClassCores     -> updateClassCoresAssessAndDecide
                                              td s ContextModePreliminary

                Nothing                   -> updateClassCoresAssessAndDecide
                                              td s ContextModePreliminary

                Just NegotiationFailed    -> error "NegotiationFailed"          -- TODO

-----------------------------------------------------------------------------

updateClassCoresAssessAndDecide td s mode =
  flip (groupAssessAndDecide s) mode =<< updateClassCores td s

updateClassCores td s = do
  let extra = stateExtra s
  gen <- newGenericGroupClassesGenerator (classroomSet extra) td
                                         (groupRoleData extra)
  mbClasses <- usingNextClassCore gen
  let details = CandidateDetails . CandidateCreationDetails
                                 $ SomeClassesGenerator gen
      mbCandidate = (\cs -> mkCandidate (Set.map SomeInformationPiece cs)
                                        details
                                        (if Set.null cs then Just False else Nothing)
                     ) <$> mbClasses
  return $ fromMaybe (mkEmptyCandidate details) mbCandidate


groupAssessAndDecide :: (Num a, Ord a, Show a, Typeable a) =>
                        AgentState Group a
                     -> ACandidate a
                     -> ContextMode
                     -> IO ()
groupAssessAndDecide s c mode = assessAndDecide s mode (mkDetails c)
                              . Just . collectClasses $ candidateInfo c

generateAssessAndDecide :: (Num a, Ord a, Show a, Typeable a) =>
                            AgentState Group a
                        -> ACandidate a
                        -> ContextMode
                        -> (SomeClassesGenerator -> IO (Maybe (Set Class)))
                        -> IO ()
generateAssessAndDecide s c mode usingClassCore =
  assessAndDecide s mode (mkDetails c) =<< usingClassCore (genOf c)


assessAndDecide s mode details Nothing = decide' s
                                       $ mkEmptyCandidate details

assessAndDecide s mode details (Just classes) =
  decide' s =<< assess' mode s (mkCandidate info details Nothing)
  where info = Set.map SomeInformationPiece classes

-----------------------------------------------------------------------------

mkEmptyCandidate details = mkCandidate Set.empty details $ Just False

mkCandidate info details coh = Candidate{ candidateInfo       = info
                                        , candidateCoherent   = coh
                                        , candidateCoherence  = 0
                                        , candidateDetails    = details
                                        , candidateAssessment = []
                                      }

assess' :: (Num a, Ord a, Show a, Typeable a) =>
           ContextMode -> AgentState r a -> ACandidate a -> IO (ACandidate a)
assess' mode s = propagateThroughContexts ctxs mode
  where c = contexts s
        ctxs = [ someCtx $ obligations c
               , someCtx $ preferences c
               , someCtx $ external c
               ]

decide' :: AgentState Group a -> ACandidate a -> IO ()
decide' s candidate = do
  let extra = stateExtra s
      decider = groupDecider extra
  decision <- groupDecide decider s candidate
  atomically $ lastDecision extra `writeTVar` Just decision

startNegotiationOver :: AgentState Group a -> ACandidate a -> IO ()
startNegotiationOver = undefined                                                -- TODO


-----------------------------------------------------------------------------

mkDetails = CandidateDetails . CandidateCreationDetails
                             . SomeClassesGenerator . genOf


collectClasses :: Information -> Set Class
collectClasses = Set.fromList . collectInformation id

genOf c = let CandidateDetails (CandidateCreationDetails gen) = candidateDetails c
          in gen

someCtx :: ( Num a, Ord a, FilteringContext c b, CtxMode c ~ ContextMode
           , CtxRelationValueConvertible b a, FilteringContextConstraints c
            ) =>
           c -> SomeFilteringContext ContextMode a
someCtx = SomeFilteringContext . AFilteringContext

-----------------------------------------------------------------------------
