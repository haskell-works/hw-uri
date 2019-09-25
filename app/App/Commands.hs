module App.Commands where

import App.Commands.Cp
import App.Commands.LsPrefix
import Data.Semigroup        ((<>))
import Options.Applicative

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdCp
  <>  cmdLsPrefix
