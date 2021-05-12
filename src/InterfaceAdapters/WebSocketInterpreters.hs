module InterfaceAdapters.WebSocketInterpreters where

import qualified Network.WebSockets as WS
import Polysemy
import Polysemy.Input
import qualified UseCases.WebSocket as UC

runWStoIO ::
    (Member (Input WS.Connection) r, Member (Embed IO) r) =>
    Sem (UC.WebSocket : r) a ->
    Sem r a
runWStoIO = interpret $ \case
    UC.SendTextData msg -> do
        conn <- input
        embed $ WS.sendTextData conn msg
    UC.SendClose msg -> do
        conn <- input
        embed $ WS.sendClose conn msg
    UC.ReceiveData -> do
        conn <- input
        embed $ WS.receiveData conn