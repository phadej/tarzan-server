module Text.Tarzan.Evaluator where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Applicative
import Data.Monoid

import Text.Tarzan.Regex (RE)
import qualified Text.Tarzan.Regex as RE

import Text.Tarzan.Expr
import Text.Tarzan.Stmt

import Text.Parsec (parse)

substitute :: Map String (RE Char) -> ExprRE -> Either String ExprRE
substitute _   r@ExprRE {}     = Right r
substitute env (ExprVar var)   = case Map.lookup var env of
                                   Just re -> Right $ ExprRE re
                                   Nothing -> Left $ "undefined variable: " ++ var
substitute env (REAppend a b)  = REAppend <$> substitute env a <*> substitute env b
substitute env (REUnion a b)   = REUnion <$> substitute env a <*> substitute env b
substitute env (REKleene re)   = REKleene <$> substitute env re

unsafeEval :: ExprRE -> (RE Char)
unsafeEval (ExprRE re)     = re
unsafeEval (ExprVar var)   = error $ "unsafeEval, var found: " ++ var
unsafeEval (REAppend a b)  = unsafeEval a RE.<.> unsafeEval b
unsafeEval (REUnion a b)   = unsafeEval a <> unsafeEval b
unsafeEval (REKleene re)   = RE.kleene $ unsafeEval re

execute :: String -> String
execute = snd . foldl f (Map.empty, "") . filter g . lines
  where g ('#':_) = False
        g ""      = False
        g _       = True
        f (env, out) line = case parse stmt "" line of
                              Left err                  -> (env, out ++ "// " ++ show err ++ "\n")
                              Right (REAssign var expr) -> case unsafeEval <$> substitute env expr of
                                                             Left err    -> (env, out ++ "// " ++ err ++ "\n")
                                                             Right expr' -> (Map.insert var expr' env, out ++ firstline ++ secondline)
                                                               where firstline = "// " ++ line ++ "\n"
                                                                     secondline = "var " ++ var ++ " = /^" ++ RE.prettyRe expr' ++ "$/;\n"
  
