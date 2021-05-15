module UseCases.WebSocketManager where

import Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Domain.CryptoWatch.WS as CW
import Domain.Targets
import Domain.WebSocket
import qualified Network.WebSockets as WS
import Polysemy
import Polysemy.Async
import Polysemy.Output
import Polysemy.Trace
import qualified UseCases.WebSocket as UC

subscribeAndCloseAfter ::
    (Member (Embed IO) r, Member Async r, Member UC.WebSocket r, Member Trace r) =>
    BS.ByteString ->
    Int ->
    Sem r ()
subscribeAndCloseAfter subMsg delay = do
    trace "Sending subscribe message"
    _ <- async $ do
        embed $ threadDelay delay
        trace "Sending close message"
        UC.sendClose ("Close" :: BS.ByteString)
    UC.sendTextData subMsg

subscribeOnOpen :: (Member UC.WebSocket r, Member Trace r) => BS.ByteString -> Sem r ()
subscribeOnOpen subMsg = do
    trace "Sending subscribe message"
    UC.sendTextData subMsg

cryptoWatchWSPayload :: (Member UC.WebSocket r, Member (Output BS.ByteString) r, Member Trace r) => WSAppConfig r
cryptoWatchWSPayload = WSAppConfig onOpen onMessage
  where
    onOpen = subscribeOnOpen krakenSub
    krakenSub = CW.subscribePayload ["markets:87:trades"]
    onMessage = output

makeWSConfig :: (Member UC.WebSocket r, Member Trace r, Member (Output BS.ByteString) r) => Exchange -> WSTarget r
makeWSConfig (CryptoWatch apiKey) =
    let host = "stream.cryptowat.ch"
        port = 443
        path = "/connect?apikey=" ++ apiKey
     in WSTarget
            (WSClientConfig host port path)
            cryptoWatchWSPayload