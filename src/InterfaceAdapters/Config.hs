module InterfaceAdapters.Config where

import Data.Generics.Labels ()
import Data.Generics.Product ()
import Data.Generics.Sum ()
import GHC.Generics

data Config = Config
  { version :: String
  }
  deriving (Generic, Show)