module InterfaceAdapters.IOApp where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.Socket
import qualified Network.WebSockets as WS
import System.Environment
import UnliftIO
import Wuss

runSecureClientUnlifted ::
    MonadUnliftIO m => HostName -> PortNumber -> String -> (WS.Connection -> m a) -> m a
runSecureClientUnlifted host port str inner =
    withRunInIO $ \runInIO ->
        runSecureClient host port str (runInIO . inner)

runConfig :: WS.ClientApp () -> (BS.ByteString -> IO ()) -> (WS.Connection -> IO ())
runConfig onOpen onMessage conn = do
    _ <- forkIO $ onOpen conn
    forever $ do
        msg <- liftIO $ WS.receiveData conn
        onMessage msg

-- Out to IO ()
runWithOptions :: String -> IO ()
runWithOptions apiKey = do
    runSecureClientUnlifted host port path app
  where
    app = runConfig onOpen onMessage
    onOpen conn = WS.sendTextData conn krakenSub
    krakenSub :: BS.ByteString
    krakenSub =
        "{\"subscribe\":{\"subscriptions\":[{\"streamSubscription\":{\"resource\":\"markets:87:trades\"}}]}}"
    onMessage = BS.putStrLn
    host = "stream.cryptowat.ch"
    port = 443
    path = "/connect?apikey=" ++ apiKey

ioApp :: IO ()
ioApp = do
    apiKey <- lookupEnv "CW_API_KEY"
    case apiKey of
        Just key -> runWithOptions key
        Nothing -> fail "CW_API_KEY not found"