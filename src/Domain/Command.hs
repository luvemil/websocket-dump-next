module Domain.Command where

import Options.Generic

data Command
    = Listen
    | Get
    deriving (Generic, Show)

instance ParseRecord Command