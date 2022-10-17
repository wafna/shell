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
  it "name" $ Right [NameLit "bing", NameLit "bang", NameLit "boom"] == doParse (many literalP) "bing bang boom"
  it "integer" $ Right [IntegerLit 5, IntegerLit 6, IntegerLit 7] == doParse (many literalP) "5 6 7"
  it "char" $ Right [CharLit '5', CharLit '6', CharLit '7'] == doParse (many literalP) "'5' '6' '7'"
  it "string" $ Right [StringLit "5", StringLit "6", StringLit "7"] == doParse (many literalP) "\"5\" \"6\" \"7\""

binaryOpPSpec :: Spec
binaryOpPSpec = describe "binary operators" $ do
  it "all" $ Right [BinOpAdd, BinOpSub, BinOpMul, BinOpDiv] == doParse (many binaryOpP) "+ - * /"

exprSpec :: Spec
exprSpec = describe "expressions" $ do
    it "simple" $ Right (ExprLit (NameLit "x")) == doParse (exprP) "x"
    it "arithmetic" $ Right (ExprBinOp (ExprBinOp (ExprLit (NameLit "x")) BinOpDiv (ExprLit (NameLit "y"))) BinOpMul (ExprLit (NameLit "z"))) == doParse (exprP) "x / y * z"

main :: IO ()
main = hspec $ do
  parserSpec
  literalPSpec
  binaryOpPSpec
  exprSpec