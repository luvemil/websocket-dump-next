module Lib.Control.Retry where

import Control.Concurrent.STM
import Control.Exception

-- | TODO: add backoff strategy
restartWhile :: TVar a -> (a -> Bool) -> IO b -> IO b
restartWhile s t a =
    a `catch` onError
  where
    onError (e :: SomeException) = do
        val <- readTVarIO s
        if t val
            then restartWhile s t a
            else throw e
