{-# LANGUAGE TemplateHaskell #-}

module UseCases.WebSocket where

import Polysemy

data WebSocket msg r a where
    SendTextData :: msg -> WebSocket msg r ()
    ReceiveData :: WebSocket msg r msg
    SendClose :: msg -> WebSocket msg r ()

makeSem ''WebSocket