-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Agent.SharedSchedule.Internal.Messages
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :  experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module AUCSP.Agent.SharedSchedule.Internal.Messages where

import AUCSP.Agent.Predef0

import Data.Set (Set)

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- * Messages

data ScheduleHolderListAndReset = ScheduleHolderListAndReset deriving (Typeable, Show)
newtype ScheduleHolderClasses = ScheduleHolderClasses
      { scheduleHolderClasses :: Set Class } deriving Typeable



deriving instance NegotiatorsConstraint => Show ScheduleHolderClasses

type instance ExpectedResponse ScheduleHolderListAndReset = ScheduleHolderClasses
