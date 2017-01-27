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


data Contexts r a = Contexts{
    -- agentRole            :: r
    capabilitiesContext  :: Capabilities r a
  , beliefsContext       :: Beliefs a
  , obligationsContext   :: Obligations a
  , preferencesContext   :: Preferences a
  , externalContext      :: External a

  -- , counterpartsOf :: forall c . AbstractClass c => c -> IO [SomeAgent]
}
