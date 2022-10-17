module Parser.Internal 
  ( Parser
  , shellP
  , Literal (..)
  , literalP
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
  = NameLit String
  | IntegerLit Integer
  | StringLit String
  | CharLit Char
  deriving (Eq, Show)

data UnaryOp
  = UOBang
  deriving (Eq, Show)

data BinaryOp
  = BOAdd | BOSub | BOMul | BODiv
  deriving (Eq, Show)

data Expr
  = ExprLit Literal
  | ExprBO Expr BinaryOp Expr
  deriving (Eq, Show)

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

idP :: Parser String
idP = lexeme $ do 
  h <- letterChar <|> char '_'
  t <- many (alphaNumChar <|> char '_')
  return $ h : t

-- Literals

literalP :: Parser Literal
literalP = choice [ nameLiteral, integerLiteral, stringLiteral, charLiteral]
  where
  nameLiteral :: Parser Literal
  nameLiteral = NameLit <$> idP
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

parseExpr :: Parser Expr
parseExpr = do
  left <- parseExprHead
  (op, right) <- parseExprTail
  return $ ExprBO left op right
  where
  parseExprLit :: Parser Expr
  parseExprLit = ExprLit <$> literalP
  parseExprHead :: Parser Expr
  parseExprHead = parseExprLit
  parseExprTail :: Parser (BinaryOp, Expr)
  parseExprTail = do
    op <- parseBinaryOp
    rhs <- parseExprLit
    return (op, rhs)

-- top parser

shellP :: Parser String
shellP = do
  e <- parseExpr
  eof
  return $ show e
