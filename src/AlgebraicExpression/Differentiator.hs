module AlgebraicExpression.Differentiator(diff) where

import AlgebraicExpression.SyntaxTree (AlgebraicExpression(..))
import AlgebraicExpression.Operations (sine, cosine, (|+|), (|*|), (|^|))



diff :: String -> AlgebraicExpression -> AlgebraicExpression


diff _ (Const _) = Const 0


diff diffVars (Var ch)
  | ch `elem` diffVars = Const 1
  | otherwise      = Var ch


diff diffVars (Exp x n) = n |*| diff diffVars x |*| (x |^| (n |+| Const (-1)))


diff diffVars (Sum x1 x2) = diff diffVars x1 |+| diff diffVars x2


diff diffVars (Product x1 x2) = diff diffVars x1 |*| x2 |+| diff diffVars x2 |*| x1


diff diffVars (Sin x) = diff diffVars x |*| cosine x


diff diffVars (Cos x) = Const (-1) |*| diff diffVars x |*| sine x


diff diffVars (Tan x) = diff diffVars x |*| cosine x |^| Const (-2)
