{-# LANGUAGE PostfixOperators #-}

module AlgebraicExpression.Parser where

import AlgebraicExpression.SyntaxTree (AlgebraicExpression(..))
import AlgebraicExpression.Operations (expn, divide, multiply, substract, add)

import Parser (Parser, parse, check)
import ParserCombinators (IsMatch(..), within, maybeWithin, (<|>), (|*), (|+), (|?))
import Parsers.Char (lower, dash, whiteSpace)
import Parsers.String (spacing, withinParens)
import Parsers.Number (double)

import Data.Ratio (approxRational)
import Data.Maybe (maybeToList, isJust)
import Data.List (foldl1')



expression :: Parser AlgebraicExpression
expression =  maybeWithin whiteSpace
              (allOperations <|> element)


allOperations :: Parser AlgebraicExpression
allOperations = sumExpr <|> differenceExpr <|> productExpr <|> divisionExpr <|> expnExpr


element :: Parser AlgebraicExpression
element = foldl1' multiply . uncurry (++) <$> nonEmptyElemParser where

  nonEmptyElemParser = check "not empty" (not . null . snd) elemParser
  elemParser = do sign <- fmap (const $ Const $ -1) <$> (dash |?)
                  c <- (constant |?)
                  v <- (var |*)
                  pure (maybeToList sign, maybeToList c ++ v)


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
productExpr = operation multiply  '*'
              (withinParens allOperations <|> divisionExpr <|>
               expnExpr <|> element)


divisionExpr :: Parser AlgebraicExpression
divisionExpr = operation divide '/'
               (withinParens allOperations <|> expnExpr <|> element)


expnExpr :: Parser AlgebraicExpression
expnExpr = operation expn '^'
           (withinParens allOperations <|> element)
