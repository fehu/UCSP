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

  ResponseRef(..), AwaitingResponse(..)

, askConfirmating
, respondConfirmating

, ConfirmOrCancel (..)

) where

import Agent.Abstract

import Control.Monad



data ConfirmOrCancel = Confirm | Cancel deriving (Show, Eq)




data ResponseRef msg = ResponseRef  { respond  :: msg -> IO () }
                     | ResponseRefT { respondT :: forall resp . resp ~ ExpectedResponse msg =>
                                                  msg -> IO resp
                                    }

newtype AwaitingResponse msg = AwaitingResponse (msg, ResponseRef (ExpectedResponse msg))

instance (Show msg) => Show (AwaitingResponse msg) where
    show (AwaitingResponse (msg,_)) = show msg


-- | Ask agents with 'msg', while 'cond' holds.
--   Send 'cancel' to those already sent to, if any fails.

askConfirmating :: ( Message msg, Message resp, Message cmsg
                   , ExpectedResponse msg ~ AwaitingResponse resp
                   , ExpectedResponse resp ~ cmsg
                   ) =>
                   [AgentRef] -> msg -> (resp -> Bool) -> cmsg -> cmsg -> IO Bool

askConfirmating [] _ _ _ _ = return False
askConfirmating refs msg cond confirm cancel
    = askConfirmating' refs [] msg cond confirm cancel

askConfirmating' (ref:rest) sent msg cond confirm cancel =
    do AwaitingResponse (resp, respf) <- ref `ask` msg
       if cond resp
          then askConfirmating' rest (respf:sent) msg cond confirm cancel
          else do forM_ sent (`respond` cancel)
                  return False

askConfirmating' [] sent _ _ _ confirm =
    do forM_ sent (`respond` confirm)
       return True

respondConfirmating :: ( Message msg, Message resp, Message cmsg
                       , ExpectedResponse msg ~ AwaitingResponse resp
                       , ExpectedResponse resp ~ cmsg
                       ) => (msg -> IO resp)
                         -> (cmsg -> Bool)
                         -> (msg -> resp -> cmsg -> IO ())
                         -> msg
                         -> IO (AwaitingResponse resp)

respondConfirmating f isConfirmed onConfirm msg = do
    resp <- f msg
    return $ AwaitingResponse (resp, ResponseRef $
            \msg' -> when (isConfirmed msg') (onConfirm msg resp msg'))



