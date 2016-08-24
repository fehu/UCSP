-----------------------------------------------------------------------------
--
-- Module      :  Agent.Ask
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  kdn.kovalev@gmail.com
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Agent.Ask (

  askWithConfirmation

) where

import Agent.Abstract

import Control.Monad

-- | Ask agents with 'msg', while 'cond' holds.
--   Send 'cancel' to those already sent to, if any fails.

askWithConfirmation :: ( Message msg, Message resp, Message confirm, Message cancel
                       , ExpectedResponse msg ~ resp
                       ) =>
                       [AgentRef] -> msg -> (resp -> Bool) -> confirm -> cancel -> IO Bool

askWithConfirmation [] _ _ _ _ = return False
askWithConfirmation refs msg cond confirm cancel
    = askWithConfirmation' refs [] msg cond confirm cancel

askWithConfirmation' (ref:rest) sent msg cond confirm cancel =
    do resp <- ref `ask` msg
       if cond resp
          then askWithConfirmation' rest (ref:sent) msg cond confirm cancel
          else do forM_ sent (`send` cancel)
                  return False

askWithConfirmation' [] sent _ _ _ confirm =
    do forM_ sent (`send` confirm)
       return True

