{-# LANGUAGE TemplateHaskell #-}

module UseCases.WebSocket where

import qualified Network.WebSockets as WS
import Polysemy

data WebSocket r a where
    SendTextData :: WS.WebSocketsData b => b -> WebSocket r ()
    ReceiveData :: WS.WebSocketsData b => WebSocket r b
    SendClose :: WS.WebSocketsData b => b -> WebSocket r ()

makeSem ''WebSocket