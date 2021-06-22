module TestPrelude
  ( module MyPrelude,
    module Test.Tasty,
    module Test.Tasty.HUnit,
    module Test.Tasty.QuickCheck,
    module TestPrelude,
  )
where

import Control.Arrow (left)
import Data.Either (isLeft)
import MyPrelude hiding (assert)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Megaparsec (Parsec, ShowErrorComponent, errorBundlePretty, parse)

-- | Like string but showing it just returns the string and doesn't escape it.
-- Needed to print out prettier error messages.
newtype Verbatim = Verbatim String
  deriving (Eq, Ord)

instance Show Verbatim where
  show (Verbatim s) = s

testParserParses ::
  (Eq a, Eq e, Show e, ShowErrorComponent e, Show a) =>
  Parsec e Text a ->
  Text ->
  a ->
  Assertion
testParserParses parser input expected =
  Right expected @=? left (Verbatim . errorBundlePretty) (parse parser "<test>" input)

testParserFails ::
  (Eq a, Show a, Show e) => Parsec e Text a -> Text -> String -> Assertion
testParserFails parser input msg =
  let parseResult = parse parser "<test>" input
   in isLeft parseResult
        @? ( "expected parser failure because: " <> msg <> "\n"
               <> "parsed result was: "
               <> show parseResult
           )
