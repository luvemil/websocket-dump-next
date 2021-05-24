{-# LANGUAGE TypeOperators #-}

module Domain.Command where

import Options.Generic

data Command
    = Listen
        { outputDir :: Maybe String
        , filePrefix :: String
        }
    | Get
    deriving (Generic, Show)

instance ParseRecord Command