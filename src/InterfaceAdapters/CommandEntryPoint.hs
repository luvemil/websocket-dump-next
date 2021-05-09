module InterfaceAdapters.CommandEntryPoint where

import qualified Domain.Types as Dom
import Polysemy
import UseCases.CommandInterpreter

commandEntryPoint :: Member CommandInterpreter r => Dom.Command -> Sem r ()
commandEntryPoint = runCommand