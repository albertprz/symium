module CommandLine.Message where

import Data.List(intercalate)


availableCommandsMessage :: String
availableCommandsMessage = showSequence ["simplify", "eval", "diff", "help"]

availableOptsMessage :: String
availableOptsMessage = showSequence $ ("--" ++) <$> ["noSimplify"]


inputMessage :: String -> String
inputMessage str = "Input " ++ str ++ ":"

helpMessage :: String
helpMessage =  unlines ["Utility to perform simple operations on algebraic expressions",
                        "Available Commands: " ++ availableCommandsMessage,
                        "Available Options:  " ++ availableOptsMessage]

unknownCommandMessage :: String
unknownCommandMessage =  unlines ["Unknown command. Please try again one of the following",
                                  "Available Commands: " ++ availableCommandsMessage]

parsingErrorMessage :: String -> String
parsingErrorMessage str = "The given " ++ str ++ " could not be parsed. " ++
                          "Please try again"


showSequence :: [String] -> String
showSequence strs = "(" ++ intercalate ", " strs ++ ")"
