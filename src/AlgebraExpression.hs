module AlgebraExpression where

data AlgebraicExpression = Constant Rational | Variable Char |
                           UnaryOperation UnaryOperator AlgebraicExpression |
                           BinaryOperation BinaryOperator AlgebraicExpression AlgebraicExpression

data UnaryOperator  = Sine | Cosine | Tangent
data BinaryOperator = Sum | Difference | Multiplication | Division |
                      Exponential
