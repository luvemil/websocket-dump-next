{-# LANGUAGE TemplateHaskell #-}

module UseCases.Polysemy.Concurrent where

import Polysemy

data Concurrent r a where
    Delay :: Int -> Concurrent r ()

makeSem ''Concurrent