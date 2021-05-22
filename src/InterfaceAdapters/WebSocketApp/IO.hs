module InterfaceAdapters.WebSocketApp.IO where

import Control.Concurrent.STM
import Control.Lens.Operators
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromMaybe)
import Domain.Targets
import Domain.WebSocket
import InterfaceAdapters.FileOutput
import InterfaceAdapters.Interpreters.Concurrent
import InterfaceAdapters.WebSocketApp.Builder
import InterfaceAdapters.WebSocketInterpreters
import Lib.Control.Retry
import Polysemy (embed, runM)
import Polysemy.Async (asyncToIO)
import Polysemy.Input (runInputConst)
import Polysemy.Output
import Polysemy.Trace
import qualified System.Directory as SysDir
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
    , outputDirectory :: FilePath
    , outputFile :: FilePath
    }

-- Out to IO ()
runWithOptions :: WSClientOptions -> IO ()
runWithOptions (WSClientOptions exchange globalChan isRunning dir filePrefix) = do
    (backoffConfig, isOnVar) <- makeExponentialBackoff (1000 * 1000) False (Just $ 180 * 1000 * 1000)
    SysDir.createDirectoryIfMissing True dir
    outputHandleVar <- newTMVarIO =<< outputFileHandle dir filePrefix ".out"
    restartWithBackoff backoffConfig isRunning (== WSOn) (main isOnVar outputHandleVar)
  where
    main isOnVar outputHandleVar = runSecureClient host port path (app isOnVar outputHandleVar)
    wsTarget = UC.makeWSConfig exchange
    (WSClientConfig host port path) = clientConfig wsTarget
    wsApp = makeWSApp $ appConfig wsTarget
    app isOnVar outputHandleVar conn =
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
            -- Input (TMVar Handle)
            & runInputConst outputHandleVar
            -- Input Network.WebSocket.Connection
            & runInputConst conn
            -- Async
            & asyncToIO
            -- Embed STM
            & runSTMtoIO
            -- Embed IO
            & runM
    logMessages msg = withHandle (\h -> embed $ BS.hPutStrLn h msg)

myWebSocketApp ::
    -- | Output dir
    Maybe String ->
    -- | Output file
    String ->
    IO ()
myWebSocketApp dirMaybe file = do
    apiKey <- lookupEnv "CW_API_KEY"
    mainChannel <- newBroadcastTChanIO
    isRunning <- newTVarIO WSOn
    let handleClose = do
            putStrLn "Shutting down gracefully"
            atomically $ do
                writeTVar isRunning WSOff
                writeTChan mainChannel (-1 :: Int)
        dir = fromMaybe "output" dirMaybe
    _ <- installHandler keyboardSignal (Catch handleClose) Nothing
    case apiKey of
        Just key -> runWithOptions (WSClientOptions (CryptoWatch key) mainChannel isRunning dir file)
        Nothing -> fail "CW_API_KEY not found"