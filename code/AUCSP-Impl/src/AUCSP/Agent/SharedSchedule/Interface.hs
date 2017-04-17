-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.SharedSchedule.Interface
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module AUCSP.Agent.SharedSchedule.Interface where

import AUCSP.Agent.Messages

import Data.Set (Set)
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Control.Monad (forM)
import Control.Arrow ( (&&&), first )

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- * Role

data SharedSchedule   = SharedSchedule   deriving (Show, Eq, Ord, Typeable)
data ScheduleObserver = ScheduleObserver deriving (Show, Eq, Ord, Typeable)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- * Interface

newtype Schedule = Schedule (Set Class) deriving Typeable
deriving instance NegotiatorsConstraint => Show Schedule

newtype ScheduleInterface = ScheduleInterface {
  tryPutCandidate :: forall a d . ( Typeable a, Typeable d
                                  , Show (Candidate a d)
                                    ) =>
                     Candidate a d -> IO (Response PutCandidateResult)
  }

newScheduleInterface :: AgentRef' -> ScheduleInterface
newScheduleInterface ref = ScheduleInterface $
                         \c -> ref `ask` TryPutCandidate (SomeCandidate c)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- * Messages (Private)
newtype TryPutClasses = TryPutClasses (Set Class) deriving Typeable
data PutClassesResult = PutClassesSuccess | PutClassesConflict (Set Class)
    deriving Typeable

deriving instance NegotiatorsConstraint => Show TryPutClasses
deriving instance NegotiatorsConstraint => Show PutClassesResult

type instance ExpectedResponse TryPutClasses = AwaitingConfirmation PutClassesResult

putClassesConflicts :: PutClassesResult -> Set Class
putClassesConflicts (PutClassesConflict s) = s
putClassesConflicts _                      = Set.empty

-----------------------------------------------------------------------------

newtype GetClassesOfGroup = GetClassesOfGroup (AgentRefOfRole Group)
  deriving (Typeable, Show)
newtype ClassesOfGroup = ClassesOfGroup { classesOfGroup :: Set Class }
  deriving Typeable

deriving instance NegotiatorsConstraint => Show ClassesOfGroup

type instance ExpectedResponse GetClassesOfGroup = ClassesOfGroup

-----------------------------------------------------------------------------

data CandidateChange = CandidateAdded | CandidateRemoved
    deriving (Typeable, Show, Eq)
newtype CandidatesChanges = CandidatesChanges (Map (AgentRefOfRole Group) CandidateChange)
    deriving (Typeable, Show)

boolCandidateChange = (==) CandidateAdded

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- * State

-- | Holds references to internal 'ScheduleHolder's
data SharedScheduleState = SharedScheduleState (Map Classroom AgentRef')
                                               (AgentRefOfRole ScheduleObserver)

-----------------------------------------------------------------------------
-- * Implementation: SharedSchedule

instance RoleName SharedSchedule where roleName = show
instance AgentRole SharedSchedule where
  type RoleResult SharedSchedule = ()
  type RoleState  SharedSchedule = SharedScheduleState
  type RoleArgs   SharedSchedule = ( Map Classroom AgentRef'
                                   , AgentRefOfRole ScheduleObserver)


sharedScheduleDescriptor :: NegotiatorsConstraint =>
                            Bool -> Int -> GenericRoleDescriptor SharedSchedule
sharedScheduleDescriptor debug n = genericRoleDescriptor SharedSchedule
  $ \args -> return GenericAgentDescriptor {
      agName = "SharedSchedule-Interface-" ++ show n
    , agDebug = debug
    , initialState = return $ uncurry SharedScheduleState args
    , messageHandling = MessageHandling{
        msgHandle = selectMessageHandler []
      , msgRespond = selectResponse [
            mbResp $ \i (TryPutCandidate c) ->
                        pure <$> sharedScheduleTryPutCandidate (agentState i) c
          ]
      }
    , action = agentNoAction
    , emptyResult = EmptyResult
    }

-- | A candidate of some GROUP agent is a Set of group's classes. Not assessed.
-- 1. Ask each holder to report group's classes.
-- 2. Combine classes into candidate.
sharedScheduleTryGetCandidate :: NegotiatorsConstraint =>
  SharedScheduleState -> KnownAgent Group -> IO SomeCandidate
sharedScheduleTryGetCandidate (SharedScheduleState refByRoom _) g =
  do resps <- forM (Map.elems refByRoom)
            $ \holder -> holder `ask` GetClassesOfGroup (knownRef g)
     mbClasses <- waitResponses resps
     let classes = maybe Set.empty (Set.unions . map classesOfGroup) mbClasses
     return . SomeCandidate $ Candidate (Set.map SomeInformationPiece classes)
                              Nothing (0 :: Int) NoDetails []

-- | Try to put a candidate into schedule, return conflictng candidates
--   (not assessed) in case of failure.
-- 1. Group candidate's classes by classroom
-- 2. Aks the corresponding holders to put classes.
-- 3. Collect results.
-- 4. Get conflicting groups.
-- 5. Get their candidates and return as response.
sharedScheduleTryPutCandidate :: NegotiatorsConstraint =>
  SharedScheduleState -> SomeCandidate -> IO PutCandidateResult
sharedScheduleTryPutCandidate s@(SharedScheduleState refByRoom o) c =
  do let classesByRoom = groupBy classRoom $ someCandidateClasses c
         err = fail $ "Failed to receive response from some ScheduleHolder(s) "
                   ++ "at `sharedScheduleTryPutCandidate`."
         combineResponses l = do
             let (results, respond) = unzip l
                 conflictClasses = foldr (Set.union . putClassesConflicts)
                                         Set.empty results
                 conflictGroups = Set.map classGroup conflictClasses
                 candidateGroup = head . collectInformation classGroup
                                       . candidateInfo'
             conflictCandidates <- mapM (sharedScheduleTryGetCandidate s)
                                 $ Set.toList conflictGroups
             if Set.null conflictClasses
               then mapM_ respondConfirm respond
                 >> notifyObserver o [(candidateGroup c, CandidateAdded)]
                 >> return PutCandidateSuccess
               else mapM_ respondCancel respond
                 >> return (PutCandidateConflicts $ Set.fromList conflictCandidates)
     resps <- forM classesByRoom $
              \(r, cs) -> let Just holder = Map.lookup r refByRoom
                          in holder `ask` TryPutClasses (Set.fromList cs)
     result <- mapM combineResponses =<< waitResponses resps
     maybe err return result


notifyObserver :: NegotiatorsConstraint =>
                  AgentRefOfRole ScheduleObserver
               -> [(KnownAgent Group, CandidateChange)]
               -> IO ()
notifyObserver o = send o . CandidatesChanges . Map.fromList
                                              . map (first knownRef)


-- sharedScheduleRemoveCandidate :: (NegotiatorsConstraint) =>
--   SharedScheduleState -> KnownAgent Group -> IO ()
-- sharedScheduleRemoveCandidate = undefined


groupBy :: (Ord k) => (a -> k) -> [a] -> [(k, [a])]
groupBy f l = do let kas = (f &&& id) <$> l
                 k <- List.nub $ fst <$> kas
                 return (k, map snd $ filter ((k ==) . fst) kas)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

someCandidateClasses :: (NegotiatorsConstraint) => SomeCandidate -> [Class]
someCandidateClasses (SomeCandidate c) = collectInformation id $ candidateInfo c


data NoDetails = NoDetails deriving (Typeable, Show)

-----------------------------------------------------------------------------
