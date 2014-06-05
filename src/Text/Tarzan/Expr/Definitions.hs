module Text.Tarzan.Expr.Definitions where

import Text.Tarzan.Regex (RE)

data ExprRE = ExprRE (RE Char)
            | ExprVar String
            | REAppend ExprRE ExprRE
            | REUnion ExprRE ExprRE
            | REKleene ExprRE
  deriving (Show)
