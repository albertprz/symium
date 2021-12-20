module CommandLine.Message where

import Data.List(intercalate)


availableCommandsMessage :: String
availableCommandsMessage = showSequence ["simplify", "eval", "diff", "help", "exit"]

availableOptsMessage :: String
availableOptsMessage = showSequence $ ("--" ++) <$> ["noSimplify"]


data CustomPrompt = Expression | DiffVars | SubstitutionExprs

data CustomPromptMessage = CustomPromptMessage {description :: String,
                                                example :: String,
                                                promptEntry :: String}

promptMessage :: CustomPrompt -> CustomPromptMessage
promptMessage Expression        = CustomPromptMessage "algebraic expression"
                                                      "(8x^2y^3 + z^3) / 8"
                                                      "Expression >> "
promptMessage DiffVars          = CustomPromptMessage "diff variables"
                                                      "x, y, z"
                                                      "Vars >> "
promptMessage SubstitutionExprs = CustomPromptMessage "substitution expressions"
                                                      "x = 8z, y = -x"
                                                      "Exprs >> "


inputMessage :: String -> String
inputMessage str = "Input " ++ str ++ ":"

helpMessage :: String
helpMessage =  unlinesConcise ["Utility to perform simple operations on algebraic expressions",
                               "Available Commands: " ++ availableCommandsMessage,
                               "Available Options:  " ++ availableOptsMessage]

unknownCommandMessage :: String
unknownCommandMessage =  unlinesConcise ["Unknown command. Please try again one of the following",
                                         "Available Commands: " ++ availableCommandsMessage,
                                         "Available Options:  " ++ availableOptsMessage]

parsingErrorMessage :: CustomPromptMessage -> String
parsingErrorMessage CustomPromptMessage {description, example} = unlinesConcise
  ["The given " ++ description ++ " could not be parsed. " ++ "Please try again",
   "Example: " ++ example]


unlinesConcise :: [String] -> String
unlinesConcise = intercalate "\n"

showSequence :: [String] -> String
showSequence strs = "(" ++ intercalate ", " strs ++ ")"
