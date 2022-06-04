module CommandLine.Parsers where

import AlgebraicExpression.Parser     (expression)
import AlgebraicExpression.SyntaxTree (AlgebraicExpression)
import CommandLine.Command            (Command (..), CommandOpts (..))
import ParserCombinators              (IsMatch (..), anySepBy, maybeWithin,
                                       (<|>), (|*))

import Parser         (ParseError, runParser)
import Parsers.Char   (comma, equal, lower)
import Parsers.String (spacing)

import           Data.Either (fromRight)
import           Data.Map    (Map)
import qualified Data.Map    as Map


parseCommand :: String -> (Command, [CommandOpts])
parseCommand input = applyParser fullCommandParser where

  applyParser = fromRight (Other, []) . (`runParser` input)

  fullCommandParser = maybeWithin spacing
                         ((,) <$> commandParser <*>
                         ((spacing *> commandOptsParser) |*))

  commandParser = (Simplify <$ is "simplify") <|>
                  (Eval     <$ is "eval")     <|>
                  (Diff     <$ is "diff")     <|>
                  (Help     <$ is "help")     <|>
                  (Exit     <$ is "exit")

  commandOptsParser = is "--" *> (NoSimplify <$ is "noSimplify")


parseVariables :: String -> Either ParseError String
parseVariables = runParser (anySepBy comma lower)


parseExprMap :: String -> Either ParseError (Map Char AlgebraicExpression)
parseExprMap input = Map.fromList <$> runParser exprMapParser input where

  exprMapParser = anySepBy comma elementParser
  elementParser = (,) <$> (lower <* maybeWithin spacing equal)
                      <*> expression
