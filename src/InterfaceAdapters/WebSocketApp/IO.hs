module InterfaceAdapters.WebSocketApp.IO where

import Control.Concurrent.STM
import Control.Exception (SomeException, catch, throw)
import Control.Lens.Operators
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Domain.CryptoWatch.WS as CW
import Domain.Targets
import Domain.WebSocket
import InterfaceAdapters.Interpreters.Concurrent
import InterfaceAdapters.WebSocketApp.Builder
import InterfaceAdapters.WebSocketInterpreters
import qualified Network.WebSockets as WS
import Polysemy (embed, runM)
import Polysemy.Async (asyncToIO)
import Polysemy.Input (runInputConst)
import Polysemy.Output
import Polysemy.Trace
import Polysemy.Writer
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

-- | TODO: add backoff strategy
restartWhile :: TVar a -> (a -> Bool) -> IO b -> IO b
restartWhile s t a =
    a `catch` onError
  where
    onError (e :: SomeException) = do
        val <- readTVarIO s
        if t val
            then restartWhile s t a
            else throw e

-- Out to IO ()
runWithOptions :: WSClientOptions -> IO ()
runWithOptions (WSClientOptions exchange globalChan isRunning) = do
    restartWhile isRunning (== WSOn) main
  where
    main = runSecureClient host port path app
    wsTarget = UC.makeWSConfig exchange
    (WSClientConfig host port path) = clientConfig wsTarget
    wsApp = makeWSApp $ appConfig wsTarget
    app conn =
        wsApp
            & runInputConst globalChan
            & runWStoIO
            & runConcurrentIO
            & traceToIO
            & runOutputSem logMessages
            & runInputConst conn
            & asyncToIO
            & runSTMtoIO
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