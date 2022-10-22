module Parser.Internal 
  ( Parser
  , shellP
  , Literal (..), literalP
  , BinaryOp (..), binaryOpP
  , Expr (..), exprP
  , module Parser.Syntax
  ) where

import Data.Text (Text)
-- import qualified Data.Text as T
import Data.Void

-- import Control.Applicative hiding (some, many)
-- import Control.Monad

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Parser.Syntax

-- import Text.Megaparsec.Debug

type Parser = Parsec Void Text

-- Discards blank space.
spaceP :: Parser ()
spaceP = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

-- Encode the convention of discarding trailing space.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceP

symbol :: Text -> Parser Text
symbol = L.symbol spaceP

parenthesized :: Parser a -> Parser a
parenthesized p = lexeme $ between (char '(') (char ')') p

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
exprP = lexeme $ do
  h <- try exprHead2P <|> exprAtomP
  t <- optional $ do 
    op <- binaryOpP
    r <- exprP
    return (op, r)
  return $ case t of 
    Nothing -> h
    Just (op, r) -> ExprBinOp op h r
  where
  -- TODO this needs to expand to cover object graphs, array indexing, etc.
  exprLitP :: Parser Expr
  exprLitP = ExprLit <$> literalP
  exprAtomP :: Parser Expr
  exprAtomP = exprParenP <|> exprLitP
  exprParenP :: Parser Expr
  exprParenP = parenthesized exprP
  exprHead2P :: Parser Expr
  exprHead2P = lexeme $ do
    l <- exprAtomP
    op <- binaryOpP
    r <- exprAtomP
    return $ ExprBinOp op l r

-- top parser
shellP :: Parser String
shellP = do
  e <- exprP
  eof
  return $ show e
