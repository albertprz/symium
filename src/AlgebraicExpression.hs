module AlgebraicExpression (AlgebraicExpression(..),
                            add, substract, multiply, divide, expn,
                            squareRoot, sine, cosine, tangent,
                            (|+|), (|-|), (|*|), (|/|), (|^|))  where

import Data.Ratio (denominator, numerator, (%))

data AlgebraicExpression = Const Rational |
                           Var Char |
                           Sum AlgebraicExpression AlgebraicExpression |
                           Product AlgebraicExpression AlgebraicExpression |
                           Exp AlgebraicExpression AlgebraicExpression |
                           Sin AlgebraicExpression |
                           Cos AlgebraicExpression |
                           Tan AlgebraicExpression
  deriving Eq


instance Show AlgebraicExpression where
  show (Const n)       = showRational n
  show (Var  ch)       = pure ch
  show (Sum x1 x2)     = showBinaryOp " + " True x1 x2
  show (Product x1 x2) = showProduct x1 x2
  show (Exp x1 x2)     = showBinaryOp "^" False x1 x2
  show (Sin x)         = showUnaryOp  "sin" x
  show (Cos x)         = showUnaryOp  "cos" x
  show (Tan x)         = showUnaryOp  "tan" x


showProduct :: AlgebraicExpression -> AlgebraicExpression -> String
showProduct (Const n1) (Const n2)     = show . Const $ n1 * n2
showProduct x1 @ (Var _) x2 @ (Var _) = show x1 ++ show x2
showProduct n @ (Const _) x @ (Var _) = show n ++ show x
showProduct x @ (Var _) n @ (Const _) = show n ++ show x
showProduct x1 x2                     = showBinaryOp " * " True x1 x2

showBinaryOp :: (Show a1, Show a2) => String -> Bool -> a1 -> a2 -> String
showBinaryOp operator parens x1 x2 = (if parens then "(" else "") ++
                                     show x1 ++ operator ++ show x2 ++
                                     (if parens then ")" else "")


showUnaryOp :: Show a => String -> a -> String
showUnaryOp operator x = operator ++ "(" ++ show x ++ ")"

showRational :: Rational -> String
showRational n
  | denominator n == 1 = show $ numerator n
  | otherwise          = show (numerator n) ++ "/" ++ show (denominator n)



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
