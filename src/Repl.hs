module Repl where

import AST
import Control.Effect.Error
import Control.Effect.Readline
import Evaluator
import MyPrelude
import Parser
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

quit :: Eff (Embed IO) m => m ()
quit = do
  embed $ say "Goodbye!"
  embed exitSuccess

parseCommand ::
  Eff (Error InterpreterError) m =>
  String ->
  m Command
parseCommand =
  fromEitherVia (PE . ParseError)
    . parse (s *> pCommand <* eof) "<interactive>"
    . pack

interpretCommand ::
  Effs [Embed IO, Error InterpreterError, Readline] m =>
  Command ->
  m ()
interpretCommand = \case
  Quit -> quit
  Parse t -> embed $ sayShow t
  TypeCheck t -> do
    t' <- mapError #_TE $ inferType t
    embed $ sayShow t'
  RawEvaluate t -> do
    t' <- mapError #_RE $ nf t
    embed $ sayShow t'
  Evaluate t -> do
    ty <- mapError #_TE $ inferType t
    t' <- mapError #_RE $ nf t
    embed $ say $ tshow t' <> " : " <> tshow ty

handleErrors ::
  (Eff (Embed IO) m, Threaders '[ErrorThreads] m p) =>
  ErrorC InterpreterError m () ->
  m ()
handleErrors = either (embed . sayErrShow . pretty) pure <=< runError

repl :: (Threaders '[ErrorThreads] m p, Effs [Embed IO, Readline] m) => m ()
repl = do
  commandStr <- getInputLine "λ> "
  case commandStr of
    Nothing -> quit
    Just commandStr' -> do
      handleErrors $ parseCommand >=> interpretCommand $ commandStr'
      repl
