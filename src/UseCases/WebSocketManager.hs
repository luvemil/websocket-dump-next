module UseCases.WebSocketManager where

import Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Domain.CryptoWatch.WS as CW
import Domain.Targets
import Domain.WebSocket
import qualified Network.WebSockets as WS
import Polysemy

subscribeAndCloseAfter :: Member (Embed IO) r => BS.ByteString -> Int -> SemClientApp r ()
subscribeAndCloseAfter subMsg delay conn = do
    embed $ putStrLn "Sending subscribe message"
    _ <- embed . forkIO $ do
        threadDelay delay
        putStrLn "Sending close message"
        WS.sendClose conn ("Close" :: BS.ByteString)
    embed $ WS.sendTextData conn subMsg

subscribeOnOpen :: Member (Embed IO) r => BS.ByteString -> SemClientApp r ()
subscribeOnOpen subMsg conn = do
    embed $ putStrLn "Sending subscribe message"
    embed $ WS.sendTextData conn subMsg

cryptoWatchWSPayload :: Member (Embed IO) r => WSAppConfig r
cryptoWatchWSPayload = WSAppConfig onOpen onMessage
  where
    onOpen = subscribeOnOpen krakenSub
    krakenSub = CW.subscribePayload ["markets:*:trades"]
    onMessage = embed . BS.putStrLn

makeWSConfig :: Member (Embed IO) r => Exchange -> WSTarget r
makeWSConfig (CryptoWatch apiKey) =
    let host = "stream.cryptowat.ch"
        port = 443
        path = "/connect?apikey=" ++ apiKey
     in WSTarget
            (WSClientConfig host port path)
            cryptoWatchWSPayload