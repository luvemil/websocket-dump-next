module Domain.CryptoWatch.WS where

import Control.Lens.Operators ((&))
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS

subscribePayload ::
    -- | resource
    [String] ->
    BS.ByteString
subscribePayload resources =
    let subscriptions =
            resources
                & map (\x -> object ["streamSubscription" .= object ["resource" .= x]])
     in encode $
            object
                [ "subscribe" .= object ["subscriptions" .= subscriptions]
                ]