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

-- import Text.Megaparsec.Debug

type Parser = Parsec Void Text

data Literal
  = NameLit String
  | IntegerLit Integer
  | StringLit String
  | CharLit Char
  | BoolLit Bool
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

-- Discards blank space.
spaceP :: Parser ()
spaceP = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

-- Combines a parser with discarding trailing space.
spaceOut :: Parser a -> Parser a
spaceOut p = p <* spaceP

-- Encode the convention of discarding trailing space.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceP

symbol :: Text -> Parser Text
symbol = L.symbol spaceP

parenthesized :: Parser a -> Parser a
parenthesized p = spaceOut $ between (char '(') (char ')') p

idP :: Parser String
idP = lexeme $ do 
  h <- letterChar <|> char '_'
  t <- many (alphaNumChar <|> char '_')
  return $ h : t

-- Literals

literalP :: Parser Literal
literalP = choice [ boolLit, nameLitP, integerLitP, stringLitP, charLitP]
  where
  boolLit :: Parser Literal
  boolLit = ((BoolLit True) <$ symbol "true") <|> ((BoolLit False) <$ symbol "false")
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
exprP = spaceOut $ do
  h <- exprAtomP
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
  exprAtomP :: Parser Expr
  exprAtomP = exprParenP <|> exprLitP
  exprParenP :: Parser Expr
  exprParenP = parenthesized exprP
  -- -- This lets us grab the head of an arithmetic expression so we can be left associative.
  -- exprHeadLitP :: Parser Expr
  -- exprHeadLitP = do
  --   h <- exprAtomP
  --   op <- binaryOpP
  --   t <- exprAtomP
  --   return $ ExprBinOp h op t
-- 
-- top parser

shellP :: Parser String
shellP = do
  e <- exprP
  eof
  return $ show e
