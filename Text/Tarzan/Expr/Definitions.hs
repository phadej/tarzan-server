module Text.Tarzan.Expr.Definitions where

import Text.Tarzan.Regex (RE)

data ExprRE = ExprRE RE
            | ExprVar String
            | REAppend ExprRE ExprRE
            | REUnion ExprRE ExprRE
            | REKleene ExprRE
  deriving (Show)
