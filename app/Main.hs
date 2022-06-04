import CommandLine.Command (Command (..), CommandOpts (..))
import CommandLine.Helpers (actionPrompt, simpleActionPrompt, writeToPrompt)
import CommandLine.Message (CustomPrompt (DiffVars, SubstitutionExprs),
                            helpMessage, unknownCommandMessage)
import CommandLine.Parsers (parseCommand, parseExprMap, parseVariables)

import AlgebraicExpression.Differentiator (diff)
import AlgebraicExpression.Evaluator      (eval)
import AlgebraicExpression.Simplifier     (simplify)
import AlgebraicExpression.SyntaxTree     (AlgebraicExpression)

import System.Exit (exitSuccess)



main :: IO ()
main = writeToPrompt ["Command >> "]               *>
      (getLine >>= uncurry execute . parseCommand) *>
       main



execute :: Command -> [CommandOpts] -> IO ()
execute Other _    = putStrLn unknownCommandMessage
execute Help _     = putStrLn helpMessage
execute Exit _     = exitSuccess
execute Simplify _ = simpleActionPrompt simplify
execute Diff opts = actionPrompt (applyAction diff opts) parseVariables DiffVars
execute Eval opts = actionPrompt (applyAction eval opts) parseExprMap   SubstitutionExprs



applyAction :: (a -> AlgebraicExpression -> AlgebraicExpression)
               -> [CommandOpts]
               -> (a -> AlgebraicExpression -> AlgebraicExpression)
applyAction action opts
  | NoSimplify `elem` opts = action
  | otherwise              = \a expr -> simplify $ action a expr
