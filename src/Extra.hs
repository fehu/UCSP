-----------------------------------------------------------------------------
--
-- Module      :  Extra
-- License     :  MIT
--
-- | Some extra definitions.
--

module Extra where

import Control.Monad

-----------------------------------------------------------------------------

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb a = (`when` a) =<< mb


