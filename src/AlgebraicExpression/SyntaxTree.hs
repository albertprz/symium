module AlgebraicExpression.SyntaxTree where


data AlgebraicExpression = Const Rational |
                           Var Char |
                           Sum AlgebraicExpression AlgebraicExpression |
                           Product AlgebraicExpression AlgebraicExpression |
                           Exp AlgebraicExpression AlgebraicExpression |
                           Sin AlgebraicExpression |
                           Cos AlgebraicExpression |
                           Tan AlgebraicExpression
  deriving (Eq)


data BinaryOperation = Addition | Multiplication | Power
data UnaryOperation  = Sine | Cosine | Tangent
