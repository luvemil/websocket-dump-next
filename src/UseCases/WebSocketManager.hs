module UseCases.WebSocketManager where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Domain.CryptoWatch.WS as CW
import Domain.Targets
import Domain.WebSocket
import qualified Network.WebSockets as WS
import Polysemy
import Polysemy.Async
import Polysemy.Input
import Polysemy.Output
import Polysemy.Trace
import qualified UseCases.Polysemy.Concurrent as UC
import qualified UseCases.WebSocket as UC

subscribeAndCloseAfter ::
    (Member UC.Concurrent r, Member Async r, Member UC.WebSocket r, Member Trace r) =>
    BS.ByteString ->
    Int ->
    Sem r ()
subscribeAndCloseAfter subMsg delay = do
    trace "Sending subscribe message"
    _ <- async $ do
        UC.delay delay
        trace "Sending close message"
        UC.sendClose ("Close" :: BS.ByteString)
    UC.sendTextData subMsg

subscribeOnOpen :: (Member UC.WebSocket r, Member Trace r) => BS.ByteString -> Sem r ()
subscribeOnOpen subMsg = do
    trace "Sending subscribe message"
    UC.sendTextData subMsg

notifyWSOn :: (Member Trace r, Member (Embed STM) r, Member (Input (TVar Bool)) r) => String -> Sem r ()
notifyWSOn msg = do
    trace msg
    isOnVar :: TVar Bool <- input
    embed $ modifyTVar' isOnVar (const True)

type CryptoWatchEffectRow r = (Member UC.WebSocket r, Member (Output BS.ByteString) r, Member Trace r, Member (Embed STM) r, Member (Input (TVar Bool)) r)

cryptoWatchWSPayload :: CryptoWatchEffectRow r => WSAppConfig r
cryptoWatchWSPayload = WSAppConfig onOpen onMessage
  where
    onOpen = do
        notifyWSOn "Connection open"
        subscribeOnOpen krakenSub
    krakenSub = CW.subscribePayload ["markets:87:trades"]
    onMessage = output

makeWSConfig :: CryptoWatchEffectRow r => Exchange -> WSTarget r
makeWSConfig (CryptoWatch apiKey) =
    let host = "stream.cryptowat.ch"
        port = 443
        path = "/connect?apikey=" ++ apiKey
     in WSTarget
            (WSClientConfig host port path)
            cryptoWatchWSPayload