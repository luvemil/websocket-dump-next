module InterfaceAdapters.Config where

import Data.Generics.Labels ()
import Data.Generics.Product ()
import Data.Generics.Sum ()
import Domain.Command
import GHC.Generics

data Config = Config
  { version :: String
  , command :: Command
  }
  deriving (Generic, Show)