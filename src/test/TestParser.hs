module Main (main) where

-- import qualified Data.Map as Map
-- import qualified Data.Either as Either
-- import Data.List (intercalate)
import Data.Text
import Data.Void

import Text.Megaparsec hiding (State)
import Parser.Internal

import Test.Hspec


doParse :: Parser a -> Text -> Either (ParseErrorBundle Text Void) a
doParse parser input = runParser parser "TEST" input

parserSpec :: Spec
parserSpec = it "parses stuff" pending

literalPSpec :: Spec
literalPSpec = describe "literals" $ do
  it "name literals" $ Right [NameLit "bing", NameLit "bang"] == doParse (many literalP) "bing bang"
  it "integer literals" $ Right [IntegerLit 5, IntegerLit 6, IntegerLit 7] == doParse (many literalP) "5 6 7"
  it "char literals" $ Right [CharLit '5', CharLit '6', CharLit '7'] == doParse (many literalP) "'5' '6' '7'"
  it "string literals" $ Right [StringLit "5", StringLit "6", StringLit "7"] == doParse (many literalP) "\"5\" \"6\" \"7\""

parseBinaryOpsSpec :: Spec
parseBinaryOpsSpec = describe "binary operators" $ do
  it "all" $ Right [BOAdd, BOSub, BOMul, BODiv] == doParse (many parseBinaryOp) "+ - * /"

main :: IO ()
main = hspec $ do
  parserSpec
  literalPSpec
  parseBinaryOpsSpec