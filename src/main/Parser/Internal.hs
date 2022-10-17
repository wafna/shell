module Parser.Internal 
  ( Parser
  , shellP
  , Literal (..), literalP
  , BinaryOp (..), binaryOpP
  , Expr (..), exprP
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
  = BinOpAdd | BinOpSub | BinOpMul | BinOpDiv
  deriving (Eq, Show)

data Expr
  = ExprLit Literal
  | ExprBinOp Expr BinaryOp Expr
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
literalP = choice [ nameLitP, integerLitP, stringLitP, charLitP]
  where
  nameLitP :: Parser Literal
  nameLitP = NameLit <$> idP
  charLitP :: Parser Literal
  charLitP = CharLit <$> (lexeme $ between (char '\'') (char '\'') L.charLiteral)
  stringLitP :: Parser Literal
  stringLitP = StringLit <$> (lexeme $ char '\"' *> manyTill L.charLiteral (char '\"'))
  integerLitP :: Parser Literal
  integerLitP = IntegerLit <$> lexeme L.decimal

binaryOpP :: Parser BinaryOp
binaryOpP = choice [opP "+" BinOpAdd, opP "-" BinOpSub, opP "*" BinOpMul, opP "/" BinOpDiv]
  where
  opP :: Text -> BinaryOp -> Parser BinaryOp
  opP s o = o <$ symbol s

-- All arithmetic is left associative and there is no precedence of operations.
exprP :: Parser Expr
exprP = do
  h <- exprParenP <|> try exprHeadLitP <|> exprLitP
  t <- optional $ do 
    op <- binaryOpP
    r <- exprP
    return (op, r)
  return $ case t of 
    Nothing -> h
    Just (op, r) -> ExprBinOp h op r
  where
  -- TODO this needs to expand to cover object graphs, array indexing, etc.
  exprLitP :: Parser Expr
  exprLitP = ExprLit <$> literalP
  exprParenP :: Parser Expr
  exprParenP = between (char '(') (char ')') exprP
  -- This lets us grab the head of an arithmetic expression so we can be left associative.
  exprHeadLitP :: Parser Expr
  exprHeadLitP = do
    h <- exprLitP
    op <- binaryOpP
    t <- exprLitP
    return $ ExprBinOp h op t

-- top parser

shellP :: Parser String
shellP = do
  e <- exprP
  eof
  return $ show e
