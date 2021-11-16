module AlgebraicExpression.Operations where

import AlgebraicExpression.SyntaxTree (AlgebraicExpression(..))

import Data.Ratio ((%))



add :: AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression
add = Sum

substract :: AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression
substract x1 x2 = Sum x1 $ Product x2 (Const $ -1)

multiply :: AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression
multiply = Product

divide :: AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression
divide x1 x2 = Product x1 $ Exp x2 (Const $ -1)

expn :: AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression
expn = Exp

squareRoot :: AlgebraicExpression -> AlgebraicExpression
squareRoot x = expn x $ Const (1 % 2)


sine :: AlgebraicExpression -> AlgebraicExpression
sine = Sin

cosine :: AlgebraicExpression -> AlgebraicExpression
cosine = Cos

tangent :: AlgebraicExpression -> AlgebraicExpression
tangent = Tan



infixl 6 |+|
(|+|) :: AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression
(|+|) = add

infixl 6 |-|
(|-|) :: AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression
(|-|) = substract

infixl 7 |*|
(|*|) :: AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression
(|*|) = multiply

infixl 7 |/|
(|/|) :: AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression
(|/|) = divide

infixr 8 |^|
(|^|) :: AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression
(|^|) = expn



mapExpr :: (AlgebraicExpression -> AlgebraicExpression) -> AlgebraicExpression -> AlgebraicExpression
mapExpr f x@(Const _)     = f x
mapExpr f x@(Var _)       = f x
mapExpr f (Sum x1 x2)     = f $ Sum (f x1) (f x2)
mapExpr f (Product x1 x2) = f $ Product (f x1) (f x2)
mapExpr f (Exp x1 x2)     = f $ Exp (f x1) (f x2)
mapExpr f (Sin x)         = f . Sin $ f x
mapExpr f (Cos x)         = f . Cos $ f x
mapExpr f (Tan x)         = f . Tan $ f x
