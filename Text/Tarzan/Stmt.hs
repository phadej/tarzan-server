module Text.Tarzan.Stmt (
  REStmt(..),
  stmt,
  ) where

import Control.Applicative hiding (many, (<|>), optional)

import Text.Parsec
import Text.Parsec.String

import Text.Tarzan.Common.Parser
import Text.Tarzan.Expr

data REStmt = REAssign String ExprRE
  deriving (Show)

stmt :: Parser REStmt
stmt = f <$> identifier <*> lexeme (string ":=") <*> expr
  where f i _ e = REAssign i e
