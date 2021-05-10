module InterfaceAdapters.CommandInterpreterIO where

import qualified Domain.Types as Dom
import InterfaceAdapters.IOApp
import Polysemy
import UseCases.CommandInterpreter

runCommandInterpreterToIO :: Member (Embed IO) r => Sem (CommandInterpreter : r) a -> Sem r a
runCommandInterpreterToIO = interpret $ \case
    RunCommand Dom.Listen -> embed ioApp
    RunCommand Dom.Get -> embed $ putStrLn "Called Get"