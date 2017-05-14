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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ConstraintKinds #-}

module AUCSP.Agent.SharedSchedule.Interface where

import AUCSP.Agent.Predef0

import Data.Set (Set)
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Control.Monad (forM)
import Control.Arrow ( (&&&), first )
import Data.Function (on)

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

-----------------------------------------------------------------------------

-- | Should be hidden, replaced by 'NegotiationRole'
type NegotiatorRoleConstraint r = ( NegotiatorOfRole r
                                  , RoleResult r ~ ()
                                  , RoleRef r ~ AgentRef () )

-- | Should be hidden, replaced by 'NegotiationRoles'
type DefaultNegotiatorsConstraints = ( NegotiatorRoleConstraint Group
                                     , NegotiatorRoleConstraint Professor)

data ScheduleInterface = ScheduleInterface {
    getClassesOf    :: forall r . NegotiatorRoleConstraint r =>
                       KnownAgent r -> IO (Response (Set Class))
  , tryPutCandidate :: forall a d . ( Typeable a, Typeable d
                                    , Show (Candidate a d)
                                      ) =>
                       Candidate a d -> IO (Response PutCandidateResult)
  }

newScheduleInterface :: (NegotiatorOfRole Group, NegotiatorOfRole Professor) =>
                        AgentRef' -> ScheduleInterface
newScheduleInterface ref = ScheduleInterface{
    getClassesOf    = \k -> ref `ask` GetClassesOf k
  , tryPutCandidate = \c -> ref `ask` TryPutCandidate (SomeCandidate c)
  }


-----------------------------------------------------------------------------

data SomeCandidate = forall a d . ( Typeable a, Typeable d
                                 , Show (Candidate a d)
                                 ) =>
   SomeCandidate (Candidate a d)

candidateInfo' :: SomeCandidate -> Information
candidateInfo' (SomeCandidate c) = candidateInfo c

instance Eq  SomeCandidate where (==)    = (==)    `on` candidateInfo'
instance Ord SomeCandidate where compare = compare `on` candidateInfo'
instance Show SomeCandidate where show (SomeCandidate c) = show c

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- * Messages (Public)

data PutCandidateResult = PutCandidateSuccess
                        | PutCandidateConflicts (Set SomeCandidate)
  deriving (Typeable, Show)

putCanidadateConflicts :: PutCandidateResult -> Set SomeCandidate
putCanidadateConflicts (PutCandidateConflicts s) = s
putCanidadateConflicts _                        = Set.empty


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

newtype TryPutCandidate = TryPutCandidate SomeCandidate deriving (Typeable, Show)
type instance ExpectedResponse TryPutCandidate = PutCandidateResult

-----------------------------------------------------------------------------

data GetClassesOf = forall r . NegotiatorRoleConstraint r =>
     GetClassesOf (KnownAgent r)
  deriving Typeable
instance Show GetClassesOf where
  show (GetClassesOf k) = "GetClassesOf " ++ knownAgentId k

type instance ExpectedResponse GetClassesOf = Set Class

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
  type RoleSysArgs SharedSchedule = ( Map Classroom AgentRef'
                                    , AgentRefOfRole ScheduleObserver)
  type RoleArgs   SharedSchedule = ()


sharedScheduleDescriptor :: DefaultNegotiatorsConstraints =>
                            Bool -> Int -> GenericRoleDescriptor SharedSchedule
sharedScheduleDescriptor debug n = genericRoleDescriptor SharedSchedule
  $ \args _ -> return GenericAgentDescriptor {
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
sharedScheduleTryGetCandidate :: DefaultNegotiatorsConstraints =>
  SharedScheduleState -> KnownAgent Group -> IO SomeCandidate
sharedScheduleTryGetCandidate (SharedScheduleState refByRoom _) g =
  do resps <- forM (Map.elems refByRoom) (`ask` GetClassesOf g)
     mbClasses <- waitResponse $ sequence resps
     let classes = maybe Set.empty Set.unions mbClasses
     return . SomeCandidate $ Candidate (Set.map SomeInformationPiece classes)
                              Nothing (0 :: Int) NoDetails []

-- | Try to put a candidate into schedule, return conflictng candidates
--   (not assessed) in case of failure.
-- 1. Group candidate's classes by classroom
-- 2. Aks the corresponding holders to put classes.
-- 3. Collect results.
-- 4. Get conflicting groups.
-- 5. Get their candidates and return as response.
sharedScheduleTryPutCandidate :: DefaultNegotiatorsConstraints =>
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
     result <- mapM combineResponses =<< waitResponse (sequence resps)
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
