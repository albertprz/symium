module AlgebraicExpression.Evaluator where

import AlgebraicExpression.SyntaxTree (AlgebraicExpression(..))
import AlgebraicExpression.Operations (mapExpr)

import Data.Map(Map)
import qualified Data.Map as Map


substitute :: Map Char Rational -> AlgebraicExpression -> AlgebraicExpression
substitute varMap x @ (Var ch)  = maybe x Const $ Map.lookup ch varMap
substitute varMap x             = x


eval :: Map Char Rational -> AlgebraicExpression -> AlgebraicExpression
eval varMap = mapExpr $ substitute varMap
