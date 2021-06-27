module Repl where

import AST
import Evaluator
import MyPrelude
import Parser
import Polysemy.Error
import Polysemy.Readline
import Prettyprinter
import System.Exit (exitSuccess)
import Text.Megaparsec (eof, parse)
import TyCheck

data Command
  = Quit
  | TypeCheck Term'
  | -- | Backdoor allowing evaluating a term without typechecking it
    RawEvaluate Term'
  | Evaluate Term'
  | Parse Term'
  deriving (Show, Eq, Generic)

data InterpreterError
  = RE RuntimeError
  | TE TypeError
  | PE ParseError
  deriving (Show, Eq, Generic)

instance Pretty InterpreterError where
  pretty = \case
    RE re -> pretty re
    TE te -> pretty te
    PE pe -> pretty pe

pCommand :: Parser Command
pCommand =
  ((keyword ":quit" <|> keyword ":q") $> Quit)
    <|> ((keyword ":type" <|> keyword ":t") $> TypeCheck <*> pTerm)
    <|> (keyword ":raw" $> RawEvaluate <*> pTerm)
    <|> ((keyword ":parse" <|> keyword ":p") $> Parse <*> pTerm)
    <|> (Evaluate <$> pTerm)

quit :: Member (Embed IO) r => Sem r ()
quit = do
  embed $ say "Goodbye!"
  embed exitSuccess

parseCommand ::
  Member (Error InterpreterError) r =>
  String ->
  Sem r Command
parseCommand =
  fromEitherVia (PE . ParseError)
    . parse (s *> pCommand <* eof) "<interactive>"
    . pack

interpretCommand ::
  Members [Embed IO, Error InterpreterError] r =>
  Command ->
  Sem r ()
interpretCommand = \case
  Quit -> quit
  Parse t -> sayShow t
  TypeCheck t -> do
    t' <- mapError TE $ inferType t
    sayShow t'
  RawEvaluate t -> do
    t' <- mapError RE $ nf t
    sayShow t'
  Evaluate t -> do
    ty <- mapError TE $ inferType t
    t' <- mapError RE $ nf t
    say $ tshow t' <> " : " <> tshow ty

handleErrors :: Member (Embed IO) r => Sem (Error InterpreterError : r) () -> Sem r ()
handleErrors = either (sayErrShow . pretty) pure <=< runError

repl :: Members [Embed IO, Readline] r => Sem r ()
repl = do
  commandStr <- getInputLine "λ> "
  case commandStr of
    Nothing -> quit
    Just commandStr' -> do
      handleErrors $ parseCommand >=> interpretCommand $ commandStr'
      repl
