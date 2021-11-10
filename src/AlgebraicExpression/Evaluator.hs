module AlgebraicExpression.Evaluator where

import AlgebraicExpression.SyntaxTree (AlgebraicExpression(..))
import AlgebraicExpression.Operations (mapExpr)

import Data.Map(Map)
import Data.Maybe(fromMaybe)
import qualified Data.Map as Map


substitute :: Map Char AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression
substitute varMap x @ (Var ch) = fromMaybe x $ Map.lookup ch varMap
substitute varMap x            = x


eval :: Map Char AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression
eval varMap = mapExpr $ substitute varMap
