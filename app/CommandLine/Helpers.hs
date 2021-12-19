module CommandLine.Helpers where

import CommandLine.Message (inputMessage, parsingErrorMessage)
import AlgebraicExpression.SyntaxTree (AlgebraicExpression)
import AlgebraicExpression.Parser (expression)
import AlgebraicExpression.Printer (showExpression)

import Parser (ParseError, runParser)

import System.IO (hFlush, stdout)


actionPrompt :: (a -> AlgebraicExpression -> AlgebraicExpression)
                -> (String -> Either ParseError a)
                -> (String, String)
                -> IO ()
actionPrompt action parser (name, promptMsg) =

  do writeToPrompt [inputMessage name, promptMsg]
     result <- parser <$> getLine
     either errorOp successOp result where

    errorOp _ = putStrLn (parsingErrorMessage name) *>
                  actionPrompt action parser (name, promptMsg)
    successOp x = simpleActionPrompt (action x)



simpleActionPrompt :: (AlgebraicExpression -> AlgebraicExpression) -> IO ()
simpleActionPrompt action = do writeToPrompt ["Input algebraic expression:",
                                              "Expression >> "]
                               expr <- runParser expression <$> getLine
                               putStrLn $ either showError showResult expr  where

  showResult = ("Result:  " ++) . showExpression . action
  showError = const $ parsingErrorMessage "algebraic expression"


writeToPrompt :: [String] -> IO ()
writeToPrompt []   = pure ()
writeToPrompt strs = putStr (unlines (init strs) ++ last strs) *>
                     hFlush stdout
