module Simplifier where

import AlgebraicExpression ( AlgebraicExpression(..) )

import Data.Ratio (denominator, numerator, approxRational)
import GHC.Float (rationalToDouble)



simplify :: AlgebraicExpression -> AlgebraicExpression
simplify = (!! 10) . iterate simplifyOnce


simplifyOnce :: AlgebraicExpression -> AlgebraicExpression
simplifyOnce x @ (Const _)   = x
simplifyOnce x @ (Var _)     = x
simplifyOnce (Sum x1 x2)     = simplifySum     (simplifyOnce x1) (simplifyOnce x2)
simplifyOnce (Product x1 x2) = simplifyProduct (simplifyOnce x1) (simplifyOnce x2)
simplifyOnce (Exp x1 x2)     = simplifyExp     (simplifyOnce x1) (simplifyOnce x2)
simplifyOnce (Sin x)         = simplifyOnce x
simplifyOnce (Cos x)         = simplifyOnce x
simplifyOnce (Tan x)         = simplifyOnce x



simplifySum :: AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression

simplifySum (Const 0) x = x
simplifySum x (Const 0) = x
simplifySum (Const n1) (Const n2) = Const $ n1 + n2

simplifySum (Product x1 y1) (Product x2 y2)
  | x1 == x2  = Product x1 $ Sum y1 y2
  | x1 == y2  = Product x1 $ Sum y1 x2
  | y1 == x2  = Product y1 $ Sum x1 y2
  | y1 == y2  = Product y1 $ Sum x1 x2

simplifySum x1 (Product x2 y2)
  | x1 == x2  = Product x1 $ Sum y2 $ Const 1
  | x1 == y2  = Product x1 $ Sum x2 $ Const 1

simplifySum (Product x1 y1) x2
  | x2 == x1  = Product x2 $ Sum y1 $ Const 1
  | x2 == y1  = Product x2 $ Sum x1 $ Const 1

simplifySum x1 x2
  | x1 == x2  = Product x1 (Const 2)


-- Simplify sum of constants
simplifySum n1 @ (Const _) (Sum n2 @ (Const _) x) =
  Sum x (Sum n1 n2)

simplifySum n1 @ (Const _) (Sum x n2 @ (Const _)) =
  Sum x (Sum n1 n2)

simplifySum (Sum n1 @ (Const _) x) n2 @ (Const _) =
  Sum x (Sum n1 n2)

simplifySum (Sum x n1 @ (Const _)) n2 @ (Const _) =
  Sum x (Sum n1 n2)


-- Simplify sum of variables
simplifySum n1 @ (Var _) (Sum n2 @ (Var _) x) =
  Sum x (Sum n1 n2)

simplifySum n1 @ (Var _) (Sum x n2 @ (Var _)) =
  Sum x (Sum n1 n2)

simplifySum (Sum n1 @ (Var _) x) n2 @ (Var _) =
  Sum x (Sum n1 n2)

simplifySum (Sum x n1 @ (Var _)) n2 @ (Var _) =
  Sum x (Sum n1 n2)

simplifySum x1 x2 = Sum x1 x2



simplifyProduct :: AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression

simplifyProduct (Const 1) x = x
simplifyProduct x (Const 1) = x
simplifyProduct (Const 0) _ = Const 0
simplifyProduct _ (Const 0) = Const 0
simplifyProduct (Const n1) (Const n2) = Const $ n1 * n2

simplifyProduct (Exp x1 y1) (Exp x2 y2)
  | x1 == x2  = Exp x1 $ Sum y1 y2
  | x1 == y2  = Exp x1 $ Sum y1 x2
  | y1 == x2  = Exp y1 $ Sum x1 y2
  | y1 == y2  = Exp y1 $ Sum x1 x2

simplifyProduct x1 (Exp x2 y2)
  | x1 == x2  = Exp x1 $ Sum y2 $ Const 1
  | x1 == y2  = Exp x1 $ Sum x2 $ Const 1

simplifyProduct (Exp x1 y1) x2
  | x2 == x1  = Exp x1 $ Sum y1 $ Const 1
  | x2 == y1  = Product x2 $ Sum x1 $ Const 1

simplifyProduct x1 x2
  | x1 == x2  = Exp x1 (Const 2)


-- Simplify product of constants
simplifyProduct n1 @ (Const _) (Product n2 @ (Const _) x) =
  Product x (Product n1 n2)

simplifyProduct n1 @ (Const _) (Product x n2 @ (Const _)) =
  Product x (Product n1 n2)

simplifyProduct (Product n1 @ (Const _) x) n2 @ (Const _) =
  Product x (Product n1 n2)

simplifyProduct (Product x n1 @ (Const _)) n2 @ (Const _) =
  Product x (Product n1 n2)


-- Simplify product of variables
simplifyProduct n1 @ (Var _) (Product n2 @ (Var _) x) =
  Product x (Product n1 n2)

simplifyProduct n1 @ (Var _) (Product x n2 @ (Var _)) =
  Product x (Product n1 n2)

simplifyProduct (Product n1 @ (Var _) x) n2 @ (Var _) =
  Product x (Product n1 n2)

simplifyProduct (Product x n1 @ (Var _)) n2 @ (Var _) =
  Product x (Product n1 n2)

simplifyProduct x1 x2 = Product x1 x2



simplifyExp :: AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression

simplifyExp (Const 1) x = Const 1
simplifyExp x (Const 1) = x
simplifyExp (Const 0) _ = Const 0
simplifyExp _ (Const 0) = Const 1

simplifyExp (Const n1) (Const n2) = Const result  where
  result      = approxRational (toDecimal n1 ** toDecimal n2) 0.00001
  toDecimal n = rationalToDouble (numerator n) (denominator n)

simplifyExp n1 @ (Const _) (Exp x n2 @ (Const _)) =
  Exp x (Exp n1 n2)

simplifyExp (Exp x n1 @ (Const _)) n2 @ (Const _) =
  Exp x (Exp n1 n2)

simplifyExp (Exp x n1 @ (Var _)) n2 @ (Var _) =
  Exp x (Exp n1 n2)

simplifyExp (Exp x1 y1) x2 =
  Exp x1 (Product y1 x2)

simplifyExp x1 x2 = Exp x1 x2
