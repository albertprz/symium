module Simplifier where

import AlgebraicExpression
import Data.Ratio


simplifySum :: AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression

simplifySum (Const 0) x = x
simplifySum x (Const 0) = x
simplifySum (Const n1) (Const n2) = Const $ n1 + n2

simplifySum (Product x1 y1) (Product x2 y2)
  | x1 == x2  = Product x1 $ Sum y1 y2

simplifySum x1 (Product x2 y2)
  | x1 == x2  = Product x1 $ Sum y2 $ Const 1

simplifySum (Product x1 y1) x2
  | x1 == x2  = Product x1 $ Sum y1 $ Const 1

simplifySum x1 x2
  | x1 == x2  = Product x1 (Const 2)

simplifySum n1 @ (Const _) (Sum x n2 @ (Const _)) =
  Sum x (Sum n1 n2)

simplifySum (Sum x n1 @ (Const _)) n2 @ (Const _) =
  Sum x (Sum n1 n2)

simplifySum x1 x2 = Sum x1 x2


simplifyProd :: AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression

simplifyProd (Const 1) x = x
simplifyProd x (Const 1) = x
simplifyProd (Const 0) _ = Const 0
simplifyProd _ (Const 0) = Const 0
simplifyProd (Const n1) (Const n2) = Const $ n1 * n2

simplifyProd (Exp x1 y1) (Exp x2 y2)
  | x1 == x2  = Exp x1 $ Sum y1 y2

simplifyProd x1 (Exp x2 y2)
  | x1 == x2  = Exp x1 $ Sum y2 $ Const 1

simplifyProd (Exp x1 y1) x2
  | x1 == x2  = Exp x1 $ Sum y1 $ Const 1

simplifyProd x1 x2
  | x1 == x2  = Exp x1 (Const 2)

simplifyProd n1 @ (Const _) (Product x n2 @ (Const _)) =
  Product x (Product n1 n2)

simplifyProd (Product x n1 @ (Const _)) n2 @ (Const _) =
  Product x (Product n1 n2)

simplifyProd x1 x2 = Product x1 x2



simplifyExp :: AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression

simplifyExp (Const 1) x = Const 1
simplifyExp x (Const 1) = x
simplifyExp (Const 0) _ = Const 0
simplifyExp _ (Const 0) = Const 1
-- simplifyExp (Const n1) (Const n2) = Const $ n1 ** n2


simplifyExp (Exp x1 y1) (Exp x2 y2)
  | x1 == x2  = Exp x1 $ Product y1 y2

simplifyExp x1 (Exp x2 y2)
  | x1 == x2  = Exp x1 $ Sum y2 $ Const 1

simplifyExp (Exp x1 y1) x2
  | x1 == x2  = Exp x1 $ Sum y1 $ Const 1

simplifyExp x1 x2
  | x1 == x2  = Exp x1 (Const 2)

simplifyExp n1 @ (Const _) (Exp x n2 @ (Const _)) =
  Exp x (Exp n1 n2)

simplifyExp (Exp x n1 @ (Const _)) n2 @ (Const _) =
  Exp x (Exp n1 n2)

simplifyExp (Exp x1 y1) x2 =
  Exp x1 (Product y1 x2)

simplifyExp x1 x2 = Exp x1 x2
