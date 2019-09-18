module App.Commands where

import App.Commands.Cp
import App.Commands.PutFile
import Data.Semigroup       ((<>))
import Options.Applicative

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdCp
  <>  cmdPutFile
