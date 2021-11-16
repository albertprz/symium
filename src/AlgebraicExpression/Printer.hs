module AlgebraicExpression.Printer (showExpression) where

import AlgebraicExpression.SyntaxTree (AlgebraicExpression(..))

import Data.Ratio (denominator, numerator)



showExpression :: AlgebraicExpression -> String

showExpression (Const n)       = showRational n
showExpression (Var  ch)       = pure ch
showExpression (Sum x1 x2)     = showSum x1 x2
showExpression (Product x1 x2) = showProduct x1 x2
showExpression (Exp x1 x2)     = showExp x1 x2
showExpression (Sin x)         = showUnaryOp "sin" x
showExpression (Cos x)         = showUnaryOp "cos" x
showExpression (Tan x)         = showUnaryOp "tan" x



showSum :: AlgebraicExpression -> AlgebraicExpression -> String

showSum  x1 (Product (Const (-1)) x2)  = showBinaryOp '-' (x1, True) (x2, True)
showSum  (Product (Const (-1)) x1) x2  = showBinaryOp '-' (x2, True) (x1, True)

showSum x (Const n) | n < 0 = showBinaryOp '-' (x, True) (Const (-n), True)
showSum (Const n) x | n < 0 = showBinaryOp '-' (x, True) (Const (-n), True)

showSum x1@(Sum _ _) x2@(Sum _ _)         = showBinaryOp '+' (x1, False) (x2, False)
showSum x1@(Sum _ _) x2                   = showBinaryOp '+' (x1, False) (x2, True)
showSum x1 x2@(Sum _ _)                   = showBinaryOp '+' (x1, True)  (x2, False)
showSum x1@(Product _ _) x2@(Product _ _) = showBinaryOp '+' (x1, False) (x2, False)
showSum x1@(Product _ _) x2               = showBinaryOp '+' (x1, False) (x2, True)
showSum x1 x2@(Product _ _)               = showBinaryOp '+' (x1, True)  (x2, False)
showSum x1 x2                             = showBinaryOp '+' (x1, True)  (x2, True)


showProduct :: AlgebraicExpression -> AlgebraicExpression -> String

showProduct (Const (-1)) x  = "-" ++ showExpression x
showProduct  x (Const (-1)) = "-" ++ showExpression x

showProduct x1 x2
  | isSimpleExpr $ Product x1 x2 = mconcat $ showExpression <$> elements (Product x1 x2)

showProduct x1@(Product _ _) x2@(Product _ _) = showBinaryOp '*' (x1, False) (x2, False)
showProduct x1@(Product _ _) x2               = showBinaryOp '*' (x1, False) (x2, True)
showProduct x1 x2@(Product _ _)               = showBinaryOp '*' (x1, True)  (x2, False)
showProduct x1@(Exp _ _) x2@(Exp _ _)         = showBinaryOp '*' (x1, False) (x2, False)
showProduct x1@(Exp _ _) x2                   = showBinaryOp '*' (x1, False) (x2, True)
showProduct x1 x2@(Exp _ _)                   = showBinaryOp '*' (x1, True)  (x2, False)
showProduct x1 x2                             = showBinaryOp '*' (x1, True)  (x2, True)


showExp :: AlgebraicExpression -> AlgebraicExpression -> String
showExp x (Const (-1)) = "1 / " ++ showExpression x
showExp x1 x2
  | isSimpleExpr x2 = showExpression x1 ++ "^" ++ showExpression x2
  | otherwise       = showExpression x1 ++ " ^ " ++ "(" ++ showExpression x2 ++ ")"

showBinaryOp :: Char -> (AlgebraicExpression, Bool) -> (AlgebraicExpression, Bool) -> String
showBinaryOp operator (x1, parens1) (x2, parens2) = operand x1 parens1 ++
                                                    " " ++ pure operator ++ " " ++
                                                    operand x2 parens2   where

  operand x parens | parens && (not . isSimpleExpr) x = "(" ++ showExpression x ++ ")"
                   | otherwise                        =  showExpression x


showUnaryOp ::  String -> AlgebraicExpression -> String
showUnaryOp operator x = operator ++ "(" ++ showExpression x ++ ")"


showRational :: Rational -> String
showRational n
  | denominator n == 1 = show $ numerator n
  | otherwise          = show (numerator n) ++ "/" ++ show (denominator n)


isSimpleExpr :: AlgebraicExpression -> Bool
isSimpleExpr (Product x y) = isSimpleExpr x && isSimpleExpr y
isSimpleExpr (Var _)       = True
isSimpleExpr (Const _)     = True
isSimpleExpr _             = False


elements :: AlgebraicExpression -> [AlgebraicExpression]
elements (Product x y) = elements x ++ elements y
elements x@(Var _)     = [x]
elements x@(Const _)   = [x]
elements _             = []
