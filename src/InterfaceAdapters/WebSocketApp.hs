module InterfaceAdapters.WebSocketApp where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import qualified Data.ByteString.Lazy.Char8 as BS
import Domain.WebSocket
import qualified Network.WebSockets as WS
import Polysemy
import Polysemy.Async
import Polysemy.Input

makeWSApp ::
    (Member (Embed IO) r, Member Async r, Member (Input (TChan Int)) r) =>
    WSAppConfig r ->
    SemClientApp r ()
makeWSApp (WSAppConfig onOpen onMessage) conn = do
    onOpen conn
    _ <- async $ do
        channel <- input
        readableChannel <- embed . atomically $ dupTChan channel
        forever $ do
            msg <- embed . atomically $ readTChan readableChannel
            case msg of
                -1 -> do
                    embed $ WS.sendClose conn ("Adieu" :: BS.ByteString)
                    forever $ do
                        embed $ putStrLn "Waiting for the connection to close..."
                        embed $ threadDelay (2 * 1000 * 1000)
                _ -> pure ()
    forever $ do
        msg <- embed $ WS.receiveData conn
        onMessage msg