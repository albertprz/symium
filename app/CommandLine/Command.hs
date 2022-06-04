module CommandLine.Command where

data Command
  = Simplify
  | Eval
  | Diff
  | Help
  | Exit
  | Other
  deriving (Eq, Show)

data CommandOpts
  = NoSimplify
  deriving (Eq, Show)
