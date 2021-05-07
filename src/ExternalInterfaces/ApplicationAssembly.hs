module ExternalInterfaces.ApplicationAssembly where

import Control.Lens.Operators
import Control.Monad (forM_)
import InterfaceAdapters.Config

loadConfig :: IO Config
loadConfig = do
  pure $ Config "0.1"

runApp :: Config -> IO ()
runApp c = do
  let v = version c
  putStrLn $ "Running version " ++ v