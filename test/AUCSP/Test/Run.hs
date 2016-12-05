-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Test.Run
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--

{-# LANGUAGE TupleSections #-}

module AUCSP.Test.Run (

  execNegotiation

) where

import AUCSP.NegotiationEnvironment

import Data.Typeable

import Control.Exception (SomeException, try)
import Control.Monad


_local_DEBUG = True
localDebug scope = when _local_DEBUG . putStrLn . (("[DEBUG] " ++ scope ++ ": ") ++)

-----------------------------------------------------------------------------


-- | Creates environment and starts the negotiation.
execNegotiation :: (Fractional a, Ord a, Show a, Typeable a) =>
                DescribeNegotiation a -> IO ( NegotiationSysCtrl SomeCandidate
                                            , Either SomeException [SomeCandidate]
                                            )
execNegotiation negDescr = do
    localDebug "execNegotiation" "newIDGenerators"
    idGen <- newIDGenerators

    let agDescrs = negotiationAgentsDescriptors negDescr idGen

    localDebug "execNegotiation" "defaultAgentSystem"
    negCtrl <- defaultAgentSystem agDescrs

    localDebug "execNegotiation" "Starting Negotiation"
    startNegotiation negCtrl

    localDebug "execNegotiation" "Waiting Result"

    liftM (negCtrl, ) . try $ waitNegotiationResult negCtrl



