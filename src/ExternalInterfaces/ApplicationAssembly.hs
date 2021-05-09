module ExternalInterfaces.ApplicationAssembly where

import Control.Lens.Operators
import Control.Monad (forM_)
import InterfaceAdapters.CommandEntryPoint (commandEntryPoint)
import InterfaceAdapters.CommandInterpreterIO (runCommandInterpreterToIO)
import InterfaceAdapters.Config
import Options.Generic (getRecord)
import Polysemy

loadConfig :: IO Config
loadConfig = do
    command <- getRecord "Command"
    pure $ Config "0.1" command

runApp :: Config -> IO ()
runApp c = do
    let v = c ^. #version
        command = c ^. #command
    putStrLn $ "Running version " ++ v
    commandEntryPoint command
        & runCommandInterpreterToIO
        & runM