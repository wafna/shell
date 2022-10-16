module Parser.Internal 
  ( Parser, parseStuff
  , Literal (..)
  , parseLiteral
  , BinaryOp (..)
  , parseBinaryOp
  , Expr (..)
  ) where

import Data.Text (Text)
-- import qualified Data.Text as T
import Data.Void

-- import Control.Applicative hiding (some, many)
-- import Control.Monad

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Literal
  = IntegerLit Integer
  | StringLit String
  | CharLit Char
  deriving (Eq, Show)

data BinaryOp
  = BOAdd | BOSub | BOMul | BODiv
  deriving (Eq, Show)

data Expr
  = ExprLit Literal
  | ExprBO Expr BinaryOp Expr

spaceOut :: Parser ()
spaceOut = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

-- Encode the convention of discarding trailing space.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceOut

symbol :: Text -> Parser Text
symbol = L.symbol spaceOut

-- Literals

parseLiteral :: Parser Literal
parseLiteral = choice [ integerLiteral, stringLiteral, charLiteral]
  where
  charLiteral :: Parser Literal
  charLiteral = CharLit <$> (lexeme $ between (char '\'') (char '\'') L.charLiteral)
  stringLiteral :: Parser Literal
  stringLiteral = StringLit <$> (lexeme $ char '\"' *> manyTill L.charLiteral (char '\"'))
  integerLiteral :: Parser Literal
  integerLiteral = IntegerLit <$> lexeme L.decimal

parseBinaryOp :: Parser BinaryOp
parseBinaryOp = choice [pOp "+" BOAdd, pOp "-" BOSub, pOp "*" BOMul, pOp "/" BODiv]
  where
  pOp :: Text -> BinaryOp -> Parser BinaryOp
  pOp s o = o <$ symbol s

-- top parser

parseStuff :: Parser String
parseStuff = do
  t1 <- parseLiteral
  op <- parseBinaryOp
  t2 <- parseLiteral
  eof
  return $ show t1 ++ " " ++ show op ++ " " ++ show t2
