module InterfaceAdapters.CommandInterpreterIO where

import qualified Domain.Types as Dom
import InterfaceAdapters.WebSocketApp (myWebSocketApp)
import Polysemy
import UseCases.CommandInterpreter

runCommandInterpreterToIO :: Member (Embed IO) r => Sem (CommandInterpreter : r) a -> Sem r a
runCommandInterpreterToIO = interpret $ \case
    RunCommand Dom.Listen -> embed myWebSocketApp
    RunCommand Dom.Get -> embed $ putStrLn "Called Get"