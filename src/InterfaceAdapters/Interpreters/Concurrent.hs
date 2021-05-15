module InterfaceAdapters.Interpreters.Concurrent where

import Control.Concurrent
import Control.Concurrent.STM
import Polysemy
import Polysemy.Embed
import UseCases.Polysemy.Concurrent

runConcurrentIO :: Member (Embed IO) r => Sem (Concurrent : r) a -> Sem r a
runConcurrentIO = interpret $ \case
    Delay x -> embed $ threadDelay x

runSTMtoIO :: Member (Embed IO) r => Sem (Embed STM : r) a -> Sem r a
runSTMtoIO = runEmbedded atomically