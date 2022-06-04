module AlgebraicExpression.SyntaxTree where


data AlgebraicExpression
  = Const Rational
  | Var Char
  | Sum AlgebraicExpression AlgebraicExpression
  | Product AlgebraicExpression AlgebraicExpression
  | Exp AlgebraicExpression AlgebraicExpression
  | Sin AlgebraicExpression
  | Cos AlgebraicExpression
  | Tan AlgebraicExpression


data BinaryOperation
  = Addition
  | Multiplication
  | Power
data UnaryOperation
  = Sine
  | Cosine
  | Tangent



instance Eq AlgebraicExpression where
  (==) (Const x1) (Const x2)           = x1 == x2
  (==) (Var x1) (Var x2)               = x1 == x2
  (==) (Sum x1 y1) (Sum x2 y2)         = (x1, y1) == (x2, y2) || (y1, x1) == (x2, y2)
  (==) (Product x1 y1) (Product x2 y2) = (x1, y1) == (x2, y2) || (y1, x1) == (x2, y2)
  (==) (Exp x1 y1) (Exp x2 y2)         = (x1, y1) == (x2, y2)
  (==) (Sin x1) (Sin x2)               = x1 == x2
  (==) (Cos x1) (Cos x2)               = x1 == x2
  (==) (Tan x1) (Tan x2)               = x1 == x2
  (==) _ _                             = False
