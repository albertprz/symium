module CommandLine.Helpers where

import AlgebraicExpression.Parser     (expression)
import AlgebraicExpression.Printer    (showExpression)
import AlgebraicExpression.SyntaxTree (AlgebraicExpression)
import CommandLine.Message            (CustomPrompt (Expression),
                                       CustomPromptMessage (description, promptEntry),
                                       inputMessage, parsingErrorMessage,
                                       promptMessage)

import Parser (ParseError, runParser)

import System.IO (hFlush, stdout)



actionPrompt :: (a -> AlgebraicExpression -> AlgebraicExpression)
                -> (String -> Either ParseError a)
                -> CustomPrompt
                -> IO ()
actionPrompt action parser customPrompt =

  do writePromptMessage msg
     result <- parser <$> getLine
     either errorOp successOp result where

    msg = promptMessage customPrompt
    errorOp _ = putStrLn (parsingErrorMessage msg) *>
                  actionPrompt action parser customPrompt
    successOp x = simpleActionPrompt (action x)




simpleActionPrompt :: (AlgebraicExpression -> AlgebraicExpression) -> IO ()
simpleActionPrompt action = do writePromptMessage msg
                               expr <- runParser expression <$> getLine
                               putStrLn $ either showError showResult expr  where

  msg = promptMessage Expression
  showResult = ("Result:  " ++) . showExpression . action
  showError = const $ parsingErrorMessage msg


writePromptMessage :: CustomPromptMessage -> IO ()
writePromptMessage msg = writeToPrompt [inputMessage $ description msg, promptEntry msg]


writeToPrompt :: [String] -> IO ()
writeToPrompt []   = pure ()
writeToPrompt strs = putStr (unlines (init strs) ++ last strs) *>
                     hFlush stdout
