import AlgebraicExpression.SyntaxTree (AlgebraicExpression)
import AlgebraicExpression.Parser (expression)
import AlgebraicExpression.Printer (showExpression)
import AlgebraicExpression.Simplifier (simplify)
import AlgebraicExpression.Differentiator (diff)
import AlgebraicExpression.Evaluator (eval)

import Parser (Parser, runParser, ParseError)
import ParserCombinators (IsMatch(..), maybeWithin, (<|>), (|*))
import Parsers.Char (lower, comma, equal)
import Parsers.String (spacing)

import System.IO (hFlush, stdout)
import Data.Either (fromRight)
import qualified Data.Map as Map
import Data.Map(Map)


data Command = Simplify | Eval | Diff | Help | Other
  deriving (Eq, Show)

data CommandOpts = NoSimplify
  deriving (Eq, Show)


main :: IO ()
main = writeToPrompt ["Command >> "]               *>
      (getLine >>= uncurry execute . parseCommand) *>
       main


inputMessage :: String -> String
inputMessage str = "Input " ++ str ++ ":"

unknownCommandMessage :: String
unknownCommandMessage =  "Unknown command. Please try again"

helpMessage :: String
helpMessage =  unlines ["Utility to perform simple operations on algebraic expressions",
                        "Available Commands: (simplify, eval, diff, help)",
                        "Available Options:  (--noSimplify)"]

parseErrorMessage :: String -> String
parseErrorMessage str = "The given " ++ str ++ " could not be parsed. " ++
                        "Please try again"


execute :: Command -> [CommandOpts] -> IO ()
execute Other _ = putStrLn unknownCommandMessage
execute Help _ = putStrLn helpMessage
execute Simplify opts = actionPrompt simplify opts
execute Diff opts = executeHelper diff parseVariables
                                  "diff variables" "Vars >> " opts
execute Eval opts = executeHelper eval parseExprMap
                                 "substitution expressions" "Exprs >> " opts


applyAction :: (AlgebraicExpression -> AlgebraicExpression) -> [CommandOpts] ->
                AlgebraicExpression -> AlgebraicExpression
applyAction action opts
  | NoSimplify `elem` opts = action
  | otherwise              = simplify . action



-- Helpers --
executeHelper :: (t -> AlgebraicExpression -> AlgebraicExpression) ->
                 (String -> Either ParseError t) -> [Char] ->
                 String -> [CommandOpts] -> IO ()
executeHelper action parser name promptMsg opts =

  do writeToPrompt [inputMessage name, promptMsg]
     result <- parser <$> getLine
     either errorOp successOp result where

    errorOp _ = putStrLn (parseErrorMessage name) *>
                  executeHelper action parser name promptMsg opts
    successOp x = actionPrompt (action x) opts




actionPrompt :: (AlgebraicExpression -> AlgebraicExpression) ->  [CommandOpts] -> IO ()
actionPrompt action opts = do writeToPrompt ["Input algebraic expression:",
                                             "Expression >> "]
                              expr <- runParser expression <$> getLine
                              putStrLn $ either showError showResult expr  where

  showResult = ("Result:  " ++) . showExpression . applyAction action opts
  showError = const $ parseErrorMessage "algebraic expression"


writeToPrompt :: [String] -> IO ()
writeToPrompt []   = pure ()
writeToPrompt strs = putStr (unlines (init strs) ++ last strs) *>
                     hFlush stdout


-- Parsers --
parseVariables :: String -> Either ParseError [Char]
parseVariables = runParser (csvParser lower)


parseExprMap :: String -> Either ParseError (Map Char AlgebraicExpression)
parseExprMap input = Map.fromList <$> runParser exprMapParser input where

  exprMapParser = csvParser elementParser
  elementParser = (,) <$> (lower <* maybeWithin spacing equal)
                      <*> expression



parseCommand :: String -> (Command, [CommandOpts])
parseCommand input = applyParser fullCommandParser where

  applyParser = fromRight (Other, []) . (`runParser` input)

  fullCommandParser = maybeWithin spacing
                         ((,) <$> commandParser <*>
                         ((spacing *> commandOptsParser) |*))

  commandParser = (Simplify <$ is "simplify") <|>
                  (Eval     <$ is "eval")     <|>
                  (Diff     <$ is "diff")     <|>
                  (Help     <$ is "help")

  commandOptsParser = is "--" *> (NoSimplify <$ is "noSimplify")



csvParser :: Parser b -> Parser [b]
csvParser element = maybeWithin spacing
                      ((:) <$> element <*>
                      ((maybeWithin spacing comma *> element) |*))
