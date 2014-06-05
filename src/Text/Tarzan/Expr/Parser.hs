module Text.Tarzan.Expr.Parser (expr) where

import Control.Applicative hiding (many, (<|>), optional)

import Text.Parsec
import Text.Parsec.String

import Text.Tarzan.Common.Parser

import Text.Tarzan.Regex (RE)
import qualified Text.Tarzan.Regex as RE

import Text.Tarzan.Expr.Definitions

singleString :: Char -> Parser (RE Char)
singleString c = RE.string <$> (char c *> many cs <* char c)
  where cs = RE.escapedChar <|> noneOf (c : [])

singleQuoteString :: Parser (RE Char)
singleQuoteString = (lexeme $ singleString '\'') <?> "single quoted string"

doubleQuoteString :: Parser (RE Char)
doubleQuoteString = (lexeme $ singleString '"') <?> "double quoted string"

re :: Parser (RE Char)
re =  lexeme (char '/' *> RE.parser <* char '/') <?> "regexp literal"

literal :: Parser ExprRE
literal = ExprRE <$> (re <|> singleQuoteString <|> doubleQuoteString)

parenthized :: Parser ExprRE
parenthized = lexeme (char '(') *> expr <* lexeme (char ')')

term :: Parser ExprRE
term = literal <|> parenthized <|> (ExprVar <$> identifier)

postfix :: Parser ExprRE
postfix = f <$> term <*> (optionMaybe (lexeme $ oneOf "*+?") <?> "postfix operator, *, + or ?")
  where f r Nothing    = r
        f r (Just '*')  = REKleene r
        f r (Just '+')  = REAppend r $ REKleene r
        f r (Just '?')  = REUnion (ExprRE RE.eps) r
        f _ (Just x)    = error $ "postfix: " ++ [x]

append :: Parser ExprRE
append = foldr1 REAppend <$> some postfix

group :: Parser ExprRE
group = f <$> sepBy1 append (lexeme (char '|') <?> "alternative separator")
  where f [x] = x
        f xs  = foldr1 REUnion xs

expr :: Parser ExprRE
expr = group
