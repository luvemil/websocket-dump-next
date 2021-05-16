module InterfaceAdapters.WebSocketApp.IO where

import Control.Concurrent.STM
import Control.Lens.Operators
import qualified Data.ByteString.Lazy.Char8 as BS
import Domain.Targets
import Domain.WebSocket
import InterfaceAdapters.Interpreters.Concurrent
import InterfaceAdapters.WebSocketApp.Builder
import InterfaceAdapters.WebSocketInterpreters
import Lib.Control.Retry
import Polysemy (embed, runM)
import Polysemy.Async (asyncToIO)
import Polysemy.Input (runInputConst)
import Polysemy.Output
import Polysemy.Trace
import System.Environment
import System.Posix.Signals
import qualified UseCases.WebSocketManager as UC
import Wuss

data WSRunningState = WSOn | WSOff
    deriving (Show, Eq)

data WSClientOptions = WSClientOptions
    { exchange :: Exchange
    , globalChan :: TChan Int
    , runningState :: TVar WSRunningState
    }

-- Out to IO ()
runWithOptions :: WSClientOptions -> IO ()
runWithOptions (WSClientOptions exchange globalChan isRunning) = do
    (backoffConfig, isOnVar) <- makeExponentialBackoff (1000 * 1000) False (Just $ 180 * 1000 * 1000)
    restartWithBackoff backoffConfig isRunning (== WSOn) (main isOnVar)
  where
    main isOnVar = runSecureClient host port path (app isOnVar)
    wsTarget = UC.makeWSConfig exchange
    (WSClientConfig host port path) = clientConfig wsTarget
    wsApp = makeWSApp $ appConfig wsTarget
    app isOnVar conn =
        wsApp
            -- Input (TChan Int)
            & runInputConst globalChan
            -- Input (TVar Bool)
            & runInputConst isOnVar
            -- UseCases.WebSocket
            & runWStoIO
            -- UseCases.Polysemy.Concurrent
            & runConcurrentIO
            -- Trace
            & traceToIO
            -- Output ByteString
            & runOutputSem logMessages
            -- Input Network.WebSocket.Connection
            & runInputConst conn
            -- Async
            & asyncToIO
            -- Embed STM
            & runSTMtoIO
            -- Embed IO
            & runM
    logMessages m = embed $ BS.putStrLn m

myWebSocketApp :: IO ()
myWebSocketApp = do
    apiKey <- lookupEnv "CW_API_KEY"
    mainChannel <- newBroadcastTChanIO
    isRunning <- newTVarIO WSOn
    let handleClose = do
            putStrLn "Shutting down gracefully"
            atomically $ do
                writeTVar isRunning WSOff
                writeTChan mainChannel (-1 :: Int)
    _ <- installHandler keyboardSignal (Catch handleClose) Nothing
    case apiKey of
        Just key -> runWithOptions (WSClientOptions (CryptoWatch key) mainChannel isRunning)
        Nothing -> fail "CW_API_KEY not found"