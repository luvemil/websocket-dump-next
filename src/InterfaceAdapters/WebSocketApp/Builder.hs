module InterfaceAdapters.WebSocketApp.Builder where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import qualified Data.ByteString.Lazy.Char8 as BS
import Domain.WebSocket
import Polysemy
import Polysemy.Async
import Polysemy.Input
import qualified UseCases.WebSocket as UC

makeWSApp ::
    (Member (Embed IO) r, Member Async r, Member (Input (TChan Int)) r, Member UC.WebSocket r) =>
    WSAppConfig r ->
    Sem r ()
makeWSApp (WSAppConfig onOpen onMessage) = do
    onOpen
    _ <- async $ do
        channel <- input
        readableChannel <- embed . atomically $ dupTChan channel
        forever $ do
            msg <- embed . atomically $ readTChan readableChannel
            case msg of
                -1 -> do
                    UC.sendClose ("Adieu" :: BS.ByteString)
                    forever $ do
                        embed $ putStrLn "Waiting for the connection to close..."
                        embed $ threadDelay (2 * 1000 * 1000)
                _ -> pure ()
    forever $ do
        msg <- UC.receiveData
        onMessage msg