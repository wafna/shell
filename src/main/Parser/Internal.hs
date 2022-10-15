module Parser.Internal 
  ( Parser, parseStuff,
    Thingy (..),
    charLiteral, stringLiteral, integerLiteral
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

data Thingy
  = IntegerLit Integer
  | StringLit String
  | CharLit Char
  deriving (Eq, Show)

spaceOut :: Parser ()
spaceOut = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

-- Encode the convention of discarding trailing space.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceOut

charLiteral :: Parser Thingy
charLiteral = CharLit <$> (lexeme $ between (char '\'') (char '\'') L.charLiteral)

stringLiteral :: Parser Thingy
stringLiteral = StringLit <$> (lexeme $ char '\"' *> manyTill L.charLiteral (char '\"'))

integerLiteral :: Parser Thingy
integerLiteral = IntegerLit <$> lexeme L.decimal

parseStuff :: Parser [Thingy]
parseStuff = do
  stuff <- many $ choice
    [ integerLiteral
    , stringLiteral
    , charLiteral
    ]
  eof
  return stuff
