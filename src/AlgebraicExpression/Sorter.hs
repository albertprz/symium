module AlgebraicExpression.Sorter (sort, sortOnce) where

import AlgebraicExpression.SyntaxTree (AlgebraicExpression(..), BinaryOperation(..))


sort :: AlgebraicExpression -> AlgebraicExpression
sort = (!! 10) . iterate sortOnce


sortOnce :: AlgebraicExpression -> AlgebraicExpression
sortOnce x @ (Const _)   = x
sortOnce x @ (Var _)     = x
sortOnce (Sum x1 x2)     = uncurry Sum $
                           sortOp Addition (sortOnce x1) (sortOnce x2)
sortOnce (Product x1 x2) = uncurry Product $
                           sortOp Multiplication (sortOnce x1) (sortOnce x2)
sortOnce (Exp x1 x2)     = Exp (sortOnce x1) (sortOnce x2)
sortOnce (Sin x)         = Sin $ sortOnce x
sortOnce (Cos x)         = Cos $ sortOnce x
sortOnce (Tan x)         = Tan $ sortOnce x


sortOp :: BinaryOperation -> AlgebraicExpression -> AlgebraicExpression ->
          (AlgebraicExpression, AlgebraicExpression)
sortOp _ x1 @ (Const _) x2             = (x1, x2)
sortOp _ x1 x2 @ (Const _)             = (x2, x1)


sortOp Multiplication (Product x1 @ (Const _) y1)
                      (Product x2 @ (Const _) y2)    = (Product x1 x2, Product y1 y2)
sortOp Multiplication  x1 @ (Product (Const _) _) x2 = (x1, x2)
sortOp Multiplication x1 x2 @ (Product (Const _) _)  = (x2, x1)


sortOp Addition (Sum x1 @ (Const _) y1)
                (Sum x2 @ (Const _) y2)   = (Sum x1 x2, Sum y1 y2)
sortOp Addition x1 @ (Sum (Const _) _) x2 = (x1, x2)
sortOp Addition x1 x2 @ (Sum (Const _) _) = (x2, x1)


sortOp Multiplication x1 @ (Var ch1) (Product x2 @ (Var ch2) y2)
  | ch1 > ch2 = (x2, Product x1 y2)
  | otherwise = (x1, Product x2 y2)

sortOp Multiplication (Product x1 @ (Var ch1) y1) x2 @ (Var ch2)
  | ch1 > ch2 = (Product x2 x1, y1)
  | otherwise = (Product x1 x2, y1)

sortOp _ x1 @ (Var ch1) x2 @ (Var ch2)
  | ch1 > ch2 = (x2, x1)
  | otherwise = (x1, x2)



sortOp _ x1 x2 @ (Var _)              = (x2, x1)
sortOp _ x1 x2 @ (Sum (Var _) _)      = (x2, x1)
sortOp _ x1 x2 @ (Product (Var _) _)  = (x2, x1)


sortOp _ x1 x2 = (x1, x2)
