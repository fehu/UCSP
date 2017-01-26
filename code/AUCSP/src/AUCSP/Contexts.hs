-----------------------------------------------------------------------------
--
-- Module      :  AUCSP.Contexts
-- License     :  MIT
--
-- |
--


module AUCSP.Contexts (module X, Contexts(..)) where

import AUCSP.Context                as X
import AUCSP.Context.Combine        as X
import AUCSP.Context.InUnitInterval as X

import AUCSP.Context.Capabilities   as X
import AUCSP.Context.Beliefs        as X
import AUCSP.Context.Obligations    as X
import AUCSP.Context.Preferences    as X
import AUCSP.Context.External       as X

import AUCSP.Classes (AbstractClass)

class Contexts c a | c -> a
  where
    type ContextsRole c  :: *
    agentRole            :: c -> ContextsRole c
    capabilitiesContext  :: c -> Capabilities (ContextsRole c) a
    beliefsContext       :: c -> Beliefs a
    obligationsContext   :: c -> Obligations a
    preferencesContext   :: c -> Preferences a
    externalContext      :: c -> External a

    counterpartsOf :: (AbstractClass cl) => c -> cl -> IO [SomeAgent]
