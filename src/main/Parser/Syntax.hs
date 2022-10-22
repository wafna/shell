module Parser.Syntax where

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
  | ExprBinOp BinaryOp Expr Expr
  deriving (Eq, Show)

