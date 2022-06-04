module AlgebraicExpression.Evaluator where

import AlgebraicExpression.Operations (mapExpr)
import AlgebraicExpression.SyntaxTree (AlgebraicExpression (..))

import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)



eval :: Map Char AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression
eval varMap = (!! 10) . iterate (evalOnce varMap)


evalOnce :: Map Char AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression
evalOnce varMap = mapExpr $ substitute varMap


substitute :: Map Char AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression
substitute varMap x@(Var ch) = fromMaybe x $ Map.lookup ch varMap
substitute _      x          = x
