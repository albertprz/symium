module AlgebraicExpression where

data AlgebraicExpression = Const Rational |
                           Var Char |
                           Sum AlgebraicExpression AlgebraicExpression |
                           Product AlgebraicExpression AlgebraicExpression |
                           Exp AlgebraicExpression AlgebraicExpression |
                           Sin AlgebraicExpression |
                           Cos AlgebraicExpression |
                           Tan AlgebraicExpression
  deriving Eq
