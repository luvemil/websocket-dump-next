{-# LANGUAGE TemplateHaskell #-}

module UseCases.CommandInterpreter where

import Domain.Command
import Polysemy

data CommandInterpreter r a where
    RunCommand :: Command -> CommandInterpreter r ()

makeSem ''CommandInterpreter