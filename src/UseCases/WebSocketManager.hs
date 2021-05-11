module UseCases.WebSocketManager where

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Domain.CryptoWatch.WS as CW
import Domain.Targets
import Domain.WebSocket
import qualified Network.WebSockets as WS
import Polysemy

cryptoWatchWSPayload :: Member (Embed IO) r => WSAppConfig r
cryptoWatchWSPayload = WSAppConfig onOpen onMessage
  where
    onOpen conn = embed $ WS.sendTextData conn krakenSub
    krakenSub = CW.subscribePayload ["markets:87:trades"]
    onMessage = embed . BS.putStrLn

makeWSConfig :: Member (Embed IO) r => Exchange -> WSTarget r
makeWSConfig (CryptoWatch apiKey) =
    let host = "stream.cryptowat.ch"
        port = 443
        path = "/connect?apikey=" ++ apiKey
     in WSTarget
            (WSClientConfig host port path)
            cryptoWatchWSPayload