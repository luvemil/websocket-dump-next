module InterfaceAdapters.IOApp where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Lens.Operators
import Control.Monad (forever)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Domain.CryptoWatch.WS as CW
import Network.Socket
import qualified Network.WebSockets as WS
import Polysemy
import Polysemy.Async
import Polysemy.Input
import System.Environment
import System.Posix.Signals
import Wuss

type SemClientApp r a = WS.Connection -> Sem r a

runConfig :: Member (Embed IO) r => WS.ClientApp () -> (BS.ByteString -> Sem r ()) -> (WS.Connection -> Sem r ())
runConfig onOpen onMessage conn = do
    _ <- embed . forkIO $ onOpen conn
    forever $ do
        msg <- embed $ WS.receiveData conn
        onMessage msg

runSemConnection ::
    (Member (Embed IO) r, Member Async r, Member (Input (TChan Int)) r) =>
    -- | onOpen
    SemClientApp r () ->
    -- | onMessage
    (BS.ByteString -> Sem r ()) ->
    SemClientApp r ()
runSemConnection onOpen onMessage conn = do
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

data WSClientOptions = WSClientOptions
    { apiKey :: String
    , globalChan :: TChan Int
    }

-- Out to IO ()
runWithOptions :: WSClientOptions -> IO ()
runWithOptions (WSClientOptions apiKey globalChan) = do
    runSecureClient host port path app
  where
    app = interpretApp . runSemConnection onOpen onMessage
    interpretApp c =
        c
            & runInputConst globalChan
            & asyncToIO
            & runM
    onOpen conn = embed $ WS.sendTextData conn krakenSub
    krakenSub = CW.subscribePayload ["markets:87:trades"]
    onMessage = embed . BS.putStrLn
    host = "stream.cryptowat.ch"
    port = 443
    path = "/connect?apikey=" ++ apiKey

ioApp :: IO ()
ioApp = do
    apiKey <- lookupEnv "CW_API_KEY"
    mainChannel <- newBroadcastTChanIO
    let handleClose = do
            putStrLn "Shutting down gracefully"
            atomically $ writeTChan mainChannel (-1 :: Int)
    _ <- installHandler keyboardSignal (Catch handleClose) Nothing
    case apiKey of
        Just key -> runWithOptions (WSClientOptions key mainChannel)
        Nothing -> fail "CW_API_KEY not found"