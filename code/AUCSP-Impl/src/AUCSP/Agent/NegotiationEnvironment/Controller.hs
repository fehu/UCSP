-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.NegotiationEnvironment.Controller
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AUCSP.Agent.NegotiationEnvironment.Controller(

  NegotiationController, newNegotiationController
, negotiationSystem, notifyNegotiation
, startNegotiation, pauseNegotiation, terminateNegotiation
, getNegotiationResult, waitNegotiationResult
, createNegotiatorsOfRole -- , NegotiatorsOfRole

, NegotiationResult(..), NegotiationFailure(..)

) where

import AgentSystem
import AgentSystem.Generic hiding (AgentOfRole)
import AUCSP.Agent.NegotiatingAgent.NegotiationDefinition
import AUCSP.Agent.NegotiationEnvironment.Integration

import Data.Typeable (Typeable)
import Data.Maybe (fromJust)

import Control.Monad (forM, forM_, (<=<) )
import Control.Arrow ( (&&&), second )
import Control.Exception (SomeException)

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Concurrent.STM

-----------------------------------------------------------------------------

data NegotiationController = NegotiationController SomeAgentSystem MutableKnownAgents

newNegotiationController :: IO NegotiationController

negotiationSystem     :: NegotiationController -> SomeAgentSystem
startNegotiation      :: NegotiationController -> IO ()
pauseNegotiation      :: NegotiationController -> IO ()
terminateNegotiation  :: NegotiationController -> IO ()
notifyNegotiation     :: (Message msg) => NegotiationController -> msg -> IO ()

getNegotiationResult  :: NegotiationController -> IO (Maybe NegotiationResult)
waitNegotiationResult :: NegotiationController -> IO NegotiationResult


class (AgentRole (RoleT r a)) => NegotiatorsOfRole r a where
  createNegotiatorsOfRole :: (NegotiationRole r a) =>
                             NegotiationController
                          -> GenericRoleDescriptor (RoleT r a)
                          -> [IO (RequitedData r a)]
                          -> IO [KnownAgent r]

----------------------------------------------------------------------------
----------------------------------------------------------------------------

newNegotiationController = NegotiationController
                        <$> (SomeAgentSystem <$> newSimpleAgentSystem)
                        <*> newMutableKnownAgents

negotiationSystem (NegotiationController sys _) = sys
startNegotiation      = startAllAgents . negotiationSystem
pauseNegotiation      = pauseAllAgents . negotiationSystem
terminateNegotiation  = terminateAllAgents . negotiationSystem
notifyNegotiation c msg = mapM_ (`send` msg) <=<
                          listAgents $ negotiationSystem c


getNegotiationResult ctrl = do
  let sys = negotiationSystem ctrl
  groupResults <- sys `collectResults` RoleT Group
  profResults  <- sys `collectResults` RoleT Professor
  return $ checkResults $ groupResults ++ profResults


waitNegotiationResult ctrl = do
  let sys = negotiationSystem ctrl
  groupResults <- sys `awaitResults` RoleT Group
  profResults  <- sys `awaitResults` RoleT Professor
  return . fromJust . checkResults . map (second Just)
         $ groupResults ++ profResults

----------------------------------------------------------------------------

-- | 1. If any result is failure (negotiation or exec), collect and return failures.
--   2. If any result is undefined (Nothing), return Nothing.
--   3. Otherwise, build and return NegotiationResult.
checkResults :: [( AgentRef NegotiationPartialResult
                 , Maybe (AgentExecutionResult NegotiationPartialResult))]
             -> Maybe NegotiationResult
checkResults results | not $ null fails = Just $ negotiationResultFailure fails
                     | notReady         = Nothing
                     | otherwise        = Just $ negotiationResultSuccess succs
  where (fails, succs, notReady) = groupResults results

type AgentRefWithResult = ( AgentRef NegotiationPartialResult
                          , AgentExecutionResult NegotiationPartialResult )

groupResults :: [( AgentRef NegotiationPartialResult
                 , Maybe (AgentExecutionResult NegotiationPartialResult))]
             -> ( [SomeAgentRefWithResult], [SomeAgentRefWithResult], Bool )

groupResults = groupResults' ([], [], False)
groupResults' acc [] = acc
groupResults' (accFail, accSucc, notDefinedFlag) ( (ref, mbExecRes) : rs ) =
  let ref' = (,) $ someAgentRef ref
  in case mbExecRes
    of Just err@(Left _) -> groupResults' (ref' err:accFail, accSucc, notDefinedFlag) rs
       Just ok@(Right _) -> groupResults' (accFail, ref' ok:accSucc, notDefinedFlag) rs
       Nothing           -> groupResults' (accFail, accSucc, True) rs

----------------------------------------------------------------------------

instance ( KnownAgentsConstraints, DataForRole Group ) =>
  NegotiatorsOfRole Group a where
    createNegotiatorsOfRole = createNegotiatorsOfRole'
                                newKnownGroup varKnownGroups
                                ((`KnownAgents` return []) . return)
                                (RoleT Professor)

instance ( KnownAgentsConstraints, DataForRole Professor ) =>
  NegotiatorsOfRole Professor a where
    createNegotiatorsOfRole = createNegotiatorsOfRole'
                                newKnownProfessor varKnownProfessors
                                (KnownAgents (return []) . return)
                                (RoleT Group)


createNegotiatorsOfRole' newKnownAgent varKnownAgents newKnownAgents counterpartRole
  (NegotiationController sys globalKnown) roleD args =
  do refs <- forM args $ newAgentOfRole sys roleD
     newKnown <- sequence $
                 do (ref, arg) <- refs `zip` args
                    return $ (newKnownAgent ref . roleRequiredData) <$> arg
     atomically $ varKnownAgents globalKnown `modifyTVar` (++ newKnown)

     let upd = KnownAgentsUpdate $ newKnownAgents newKnown
     counterparts <- sys `listAgentsOfRole` counterpartRole
     forM_ counterparts (`sendPriority` upd)

     return newKnown

-----------------------------------------------------------------------------

data NegotiationResult = NegotiationResultSucess
                          (Map SomeAgentRef ( Timetable
                                            , (SomeCoherence, CandidateDetails)
                                            ))
                       | NegotiationResultFailure (Map SomeAgentRef NegotiationFailure)

data NegotiationFailure = NegotiationExecutionException SomeException
                        | NegotiationFailure                                    -- TODO: reason


type SomeAgentRefWithResult =
  (SomeAgentRef, AgentExecutionResult NegotiationPartialResult)


negotiationResultFailure :: [SomeAgentRefWithResult] -> NegotiationResult
negotiationResultFailure =
  NegotiationResultFailure . Map.fromList . map (second mkNegotiationFailure)
    where mkNegotiationFailure (Left err) = NegotiationExecutionException err
          mkNegotiationFailure (Right _)  = NegotiationFailure                  -- TODO


negotiationResultSuccess :: [SomeAgentRefWithResult] -> NegotiationResult
negotiationResultSuccess execResults = NegotiationResultSucess . Map.fromList $
  do (ref, Right (NegotiationPartialResultSuccess _ tt coh det)) <- execResults
     return (ref, (tt, (coh, det)))

-----------------------------------------------------------------------------
