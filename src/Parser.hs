-- | Parser for the language. See notes on 'lexeme' for some details on how
-- this parser differs from standard best practices.
module Parser where

import AST
import Control.Monad.Combinators.Expr
import Data.Void
import MyPrelude
import Numeric.Natural
import Prettyprinter (Pretty (..))
import Text.Megaparsec hiding (ParseError, many, sepBy1)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

newtype ParseError = ParseError {unParseError :: ParseErrorBundle Text Void}
  deriving (Show, Eq, Generic)

instance Pretty ParseError where
  pretty = pretty . errorBundlePretty . unParseError

s :: Parser ()
s = L.space space1 empty empty

-- | Unlike Text.Megaparsec.Char.Lexer's recommendations, which suggest a
-- convention where every parser consumes all whitespace after it (so that one
-- doesn't need to worry about parsing initial whitespace), we follow the
-- opposite convention where every parser consumes all of the whitespace before
-- it, which gives us the same guarantee, but allows us to require initial
-- whitespace between some terms, which is required to parse function arguments
-- for example.
--
-- With this convention it is then possible to parse applications as infix
-- operators (see 'pTerm') or require space after a keyword to avoid keywords
-- and variables running into one another (see 'separated' for example).
--
-- Note: a potentially major downside to this approach is that every lexeme
-- needs to be wrapped in try and we loose the free try-like behavior that we
-- get from Token parsers like string/symbol.
lexeme :: Parser a -> Parser a
lexeme p = try (s *> p)

symbol :: Text -> Parser Text
symbol = lexeme . string

keyword :: Text -> Parser Text
keyword t = lexeme (string t <* notFollowedBy identStartChar)

natural :: Parser Natural
natural = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | extraIdentChars :: [Char]
extraIdentChars :: String
extraIdentChars = "-_'"

identStartChar :: Parser Char
identStartChar = alphaNumChar

identChar :: Parser Char
identChar = alphaNumChar <|> oneOf extraIdentChars

-- | Identifiers with only innocuous characters may appear as barewords
bareIdentifier :: Parser Text
bareIdentifier = do
  -- we need to make sure that the identifier isn't actually a keyword by
  -- checking the keywords that could appear
  notFollowedBy pMagic
  notFollowedBy pUniverse
  lexeme $ pack <$> ((:) <$> identStartChar <*> many identChar)

preUniverse :: Parser Term'
preUniverse = "U" *> (Universe <$> L.decimal) <* notFollowedBy identStartChar

pUniverse :: Parser Term'
pUniverse = lexeme preUniverse

stringLiteral :: Parser Text
stringLiteral = lexeme $ char '"' >> pack <$> manyTill L.charLiteral (char '"')

-- | for now allowing arbitrary variable names to appear in strings. We
-- obviously might need to adapt this to a different syntax once we have
-- support for strings :P
identifier :: Parser Text
identifier = bareIdentifier <|> stringLiteral

pTerm :: Parser Term'
pTerm =
  makeExprParser
    pPrimitiveTerm
    [ [InfixL (App <$ pure ())],
      -- type annotations are non-associative because there isn't a cannonical
      -- way that they should associate, we could always make them associate
      -- one way or the other later
      [InfixN (TyAnn <$ symbol ":")],
      [InfixR (arrow <$ symbol "->")]
    ]

pPi :: Parser Term'
pPi = (piStart $> pib) <*> (identifier <* symbol ":") <*> (pTerm <* symbol ".") <*> pTerm
  where
    piStart = keyword "forall"

pLam :: Parser Term'
pLam = (lamStart $> lam) <*> (identifier <* symbol ".") <*> pTerm
  where
    lamStart = keyword "lambda"

pVar :: Parser Term'
pVar = Var <$> identifier

pMagic :: Parser Term'
pMagic = keyword "magic" $> Magic

pPrimitiveTerm :: Parser Term'
pPrimitiveTerm =
  parens pTerm
    <|> pUniverse
    <|> pMagic
    <|> pPi
    <|> pLam
    <|> pVar
