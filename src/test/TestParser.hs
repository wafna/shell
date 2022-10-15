module Main (main) where

import qualified Data.Map as Map
import qualified Data.Either as Either
import Data.List (intercalate)
import Data.Text
import Data.Void

import Text.Megaparsec hiding (State)
import Parser.Internal

import Test.Hspec


doParse :: Parser a -> String -> Text -> Either (ParseErrorBundle Text Void) a
doParse parser source input = runParser parser source input

parserSpec :: Spec
parserSpec = it "parses stuff" pending

integerLiterals :: Spec
integerLiterals = describe "integer literals" $ do
    describe "single digit" $ do
        it "foo" $ Right [IntegerLit 5, IntegerLit 6, IntegerLit 7] == doParse (many integerLiteral) "TEST" "5 6 7"
  

main :: IO ()
main = hspec $ do
  parserSpec
  integerLiterals