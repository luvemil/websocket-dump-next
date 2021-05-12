module Domain.WebSocket.Client.Types where

import qualified Data.ByteString.Lazy.Char8 as BS
import Network.Socket (HostName, PortNumber)
import Polysemy (Sem)

data WSAppConfig r = WSAppConfig
    { onOpen :: Sem r ()
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