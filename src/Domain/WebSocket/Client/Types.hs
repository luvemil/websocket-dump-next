module Domain.WebSocket.Client.Types where

import qualified Data.ByteString.Lazy.Char8 as BS
import Network.Socket (HostName, PortNumber)
import qualified Network.WebSockets as WS (Connection)
import Polysemy (Sem)

type SemClientApp r a = WS.Connection -> Sem r a

data WSAppConfig r = WSAppConfig
    { onOpen :: SemClientApp r ()
    , onMessage :: BS.ByteString -> Sem r ()
    }

data WSClientConfig = WSClientConfig
    { host :: HostName
    , port :: PortNumber
    , path :: String
    }
    deriving (Show, Eq)

data WSTarget r = WSTarget
    { clientConfig :: WSClientConfig
    , appConfig :: WSAppConfig r
    }