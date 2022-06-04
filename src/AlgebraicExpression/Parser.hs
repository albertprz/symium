module AlgebraicExpression.Parser (expression) where

import AlgebraicExpression.Operations (add, divide, expn, multiply, substract)
import AlgebraicExpression.SyntaxTree (AlgebraicExpression (..))

import Parser            (Parser, check)
import ParserCombinators (IsMatch (..), maybeWithin, (<|>), (|*), (|?))
import Parsers.Char      (lower, whiteSpace)
import Parsers.Number    (double)
import Parsers.String    (spacing, withinParens)

import Data.List  (foldl1')
import Data.Maybe (maybeToList)
import Data.Ratio (approxRational)


expression :: Parser AlgebraicExpression
expression = maybeWithin whiteSpace (allOperations <|> element)


allOperations :: Parser AlgebraicExpression
allOperations = sumExpr <|> differenceExpr <|> productExpr <|> divisionExpr <|> expnExpr


element :: Parser AlgebraicExpression
element = foldl1' multiply . uncurry (++) <$> nonEmptyElemParser where

  nonEmptyElemParser = check "not empty" (not . null . snd) elemParser

  elemParser = do sign <- fmap (const $ Const $ -1) <$> (is '-' |?)
                  ct <- (constant |?)
                  vars <- (((,) <$> var <*> ((is '^' *> constant) |?)) |*)
                  let varsWithExp = (\(x, y) -> maybe x (expn x) y) <$> vars
                  pure (maybeToList sign, maybeToList ct ++ varsWithExp)


operation :: (AlgebraicExpression -> AlgebraicExpression -> AlgebraicExpression) ->
              Char -> Parser AlgebraicExpression -> Parser AlgebraicExpression

operation ctor operator operandParser = foldl1' ctor <$> operationsParser where

  operand = maybeWithin spacing operandParser

  operationsParser = do x1 <- operand
                        is operator
                        x2 <- operand
                        xs <- ((is operator *> operand) |*)
                        pure $ [x1, x2] ++ xs


constant :: Parser AlgebraicExpression
constant = Const . (`approxRational` 0.00001) <$> double


var :: Parser AlgebraicExpression
var = Var <$> lower


sumExpr :: Parser AlgebraicExpression
sumExpr = operation add '+'
          (withinParens allOperations <|> differenceExpr <|>
           productExpr <|> divisionExpr <|> expnExpr <|> element)



differenceExpr :: Parser AlgebraicExpression
differenceExpr = operation substract '-'
                 (withinParens allOperations <|> productExpr <|>
                  divisionExpr <|> expnExpr <|> element)


productExpr :: Parser AlgebraicExpression
productExpr = operation multiply '*'
              (withinParens allOperations <|> divisionExpr <|>
               expnExpr <|> element)


divisionExpr :: Parser AlgebraicExpression
divisionExpr = operation divide '/'
               (withinParens allOperations <|> expnExpr <|> element)


expnExpr :: Parser AlgebraicExpression
expnExpr = operation expn '^'
           (withinParens allOperations <|> element)
