module Lib.Control.Retry where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception

restartWhile :: TVar a -> (a -> Bool) -> IO b -> IO b
restartWhile s t a =
    a `catch` onError
  where
    onError (e :: SomeException) = do
        val <- readTVarIO s
        if t val
            then restartWhile s t a
            else throw e

data BackoffStrategy
    = ConstantBackoff Int
    | ExponentialBackoff Int (TVar Bool) (Maybe Int) (TVar Int)

makeExponentialBackoff :: Int -> Bool -> Maybe Int -> IO (BackoffStrategy, TVar Bool)
makeExponentialBackoff delay initialOnState maxWaitMaybe = do
    counter <- newTVarIO 0
    isOnVar <- newTVarIO initialOnState
    pure $ (ExponentialBackoff delay isOnVar maxWaitMaybe counter, isOnVar)

restartWhileGeneric ::
    -- | action to perform before the restart
    IO () ->
    -- | async action to perform after s
    IO () ->
    -- | isRunning state
    TVar a ->
    -- | isRunning test
    (a -> Bool) ->
    -- | main action to retry
    IO b ->
    IO b
restartWhileGeneric pre post s t a =
    a `catch` onError
  where
    onError (e :: SomeException) = do
        val <- readTVarIO s
        if t val
            then do
                pre
                _ <- forkIO post
                restartWhile s t a
            else throw e

restartWithBackoff ::
    BackoffStrategy ->
    TVar a ->
    (a -> Bool) ->
    IO b ->
    IO b
restartWithBackoff (ConstantBackoff delay) =
    restartWhileGeneric pre post
  where
    pre = threadDelay delay
    post = pure ()
restartWithBackoff (ExponentialBackoff delay isOnVar maxWaitMaybe counterVar) = do
    restartWhileGeneric pre post
  where
    -- If an exception is caught and isOnVar is off increase the counter, else
    -- se the counter to 0, then wait for delay * 2 ^ counter
    pre = do
        wait <- atomically $ do
            isOn <- readTVar isOnVar
            if isOn
                then do
                    modifyTVar' isOnVar (const False)
                    modifyTVar' counterVar (const 0)
                else modifyTVar' counterVar (+ 1)
            counter <- readTVar counterVar
            pure $ delay * (2 ^ counter)
        let timeToWait = maybe wait (min wait) maxWaitMaybe
        threadDelay timeToWait
    post = pure ()