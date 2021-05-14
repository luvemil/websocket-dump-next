module InterfaceAdapters.WebSocketApp.IO where

import Control.Concurrent.STM
import Control.Lens.Operators
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Domain.CryptoWatch.WS as CW
import Domain.Targets
import Domain.WebSocket
import InterfaceAdapters.WebSocketApp.Builder
import InterfaceAdapters.WebSocketInterpreters
import qualified Network.WebSockets as WS
import Polysemy
import Polysemy.Async
import Polysemy.Input
import System.Environment
import System.Posix.Signals
import qualified UseCases.WebSocketManager as UC
import Wuss

data WSClientOptions = WSClientOptions
    { exchange :: Exchange
    , globalChan :: TChan Int
    }

-- Out to IO ()
runWithOptions :: WSClientOptions -> IO ()
runWithOptions (WSClientOptions exchange globalChan) = do
    runSecureClient host port path app
  where
    wsTarget = UC.makeWSConfig exchange
    (WSClientConfig host port path) = clientConfig wsTarget
    wsApp = makeWSApp $ appConfig wsTarget
    app conn =
        wsApp
            & runInputConst globalChan
            & runWStoIO
            & runInputConst conn
            & asyncToIO
            & runM

myWebSocketApp :: IO ()
myWebSocketApp = do
    apiKey <- lookupEnv "CW_API_KEY"
    mainChannel <- newBroadcastTChanIO
    let handleClose = do
            putStrLn "Shutting down gracefully"
            atomically $ writeTChan mainChannel (-1 :: Int)
    _ <- installHandler keyboardSignal (Catch handleClose) Nothing
    case apiKey of
        Just key -> runWithOptions (WSClientOptions (CryptoWatch key) mainChannel)
        Nothing -> fail "CW_API_KEY not found"