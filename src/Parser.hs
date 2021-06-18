module Parser where

import AST
import Control.Monad.Combinators.Expr
import Data.Void
import MyPrelude hiding (try)
import Numeric.Natural
import Text.Megaparsec hiding (some)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

s :: Parser ()
s = L.space space1 empty empty -- (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme s

symbol :: Text -> Parser Text
symbol = L.symbol s

natural :: Parser Natural
natural = L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | extraIdentChars :: [Char]
extraIdentChars :: String
extraIdentChars = "-._'"

identChar :: Parser Char
identChar = alphaNumChar <|> oneOf extraIdentChars

-- | Identifiers with only innocuous characters may appear as barewords
bareIdentifier :: Parser Text
bareIdentifier = L.lexeme s $ pack <$> some identChar

stringLiteral :: Parser Text
stringLiteral = L.lexeme s $ char '"' >> pack <$> manyTill L.charLiteral (char '"')

identifier :: Parser Text
identifier = bareIdentifier <|> stringLiteral

pTerm :: Parser Term'
pTerm =
  makeExprParser
    pPrimitiveTerm
    [ [InfixR (arrow <$ symbol "->")],
      -- type annotations are non-associative because there isn't a cannonical
      -- way that they should associate, we could always make them associate
      -- one way or the other later
      [InfixN (TyAnn <$ symbol ":")],
      [InfixL (App <$ s)]
    ]

pPi :: Parser Term'
pPi = (piStart $> pib) <*> (identifier <* symbol ".") <*> pTerm <*> pTerm
  where
    piStart = symbol "forall"

pLam :: Parser Term'
pLam = (lamStart $> lam) <*> (identifier <* symbol ".") <*> pTerm
  where
    lamStart = symbol "lambda"

pVar :: Parser Term'
pVar = Var <$> identifier

pPrimitiveTerm :: Parser Term'
pPrimitiveTerm =
  parens pTerm
    <|> try (symbol "U" *> (Universe <$> natural))
    <|> try (symbol "magic" $> Magic)
    <|> try pPi
    <|> try pLam
    <|> try pVar
    <|> pTerm
