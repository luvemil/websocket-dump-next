module InterfaceAdapters.WebSocketApp.Builder where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import qualified Data.ByteString.Lazy.Char8 as BS
import Domain.WebSocket
import Polysemy
import Polysemy.Async
import Polysemy.Input
import Polysemy.Trace
import qualified UseCases.Polysemy.Concurrent as UC
import qualified UseCases.WebSocket as UC

makeWSApp ::
    (Member UC.Concurrent r, Member (Embed STM) r, Member Async r, Member (Input (TChan Int)) r, Member (UC.WebSocket BS.ByteString) r, Member Trace r) =>
    WSAppConfig r ->
    Sem r ()
makeWSApp (WSAppConfig onOpen onMessage) = do
    onOpen
    _ <- async $ do
        channel <- input
        readableChannel <- embed $ dupTChan channel
        forever $ do
            msg <- embed $ readTChan readableChannel
            case msg of
                -1 -> do
                    UC.sendClose ("Adieu" :: BS.ByteString)
                    forever $ do
                        trace "Waiting for the connection to close..."
                        UC.delay (2 * 1000 * 1000)
                _ -> pure ()
    forever $ do
        msg <- UC.receiveData
        async $ onMessage msg