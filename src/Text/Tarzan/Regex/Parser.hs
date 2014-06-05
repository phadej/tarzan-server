module Text.Tarzan.Regex.Parser (escapedChar, parser, parseRe, unsafeParseRe) where

import Control.Applicative hiding (many, (<|>), optional)

import Text.Parsec
import Text.Parsec.String

import Text.Tarzan.Common.Parser

import Data.Tarzan (RE)
import qualified Data.Tarzan as RE
import Data.Monoid

reChar :: Parser (RE Char)
reChar = reCharGroup <|> reAnyChar <|> reTopSingleChar
  where reCharGroup               = (\_ a b _ -> RE.chars a b) <$> char '[' <*> reCharGroupPos <*> many reCharPair <*> char ']'
        reCharGroupPos            = option True (const False <$> char '^')
        reCharPair                = reCharPairCon <$> reSingleChar <*> optionMaybe (char '-' *> reSingleChar)
        reCharPairCon c Nothing   = (c, c)
        reCharPairCon a (Just b)  = (a, b)
        reSingleChar              = escapedChar <|> noneOf "]\\"
        reTopSingleChar           = RE.char <$> (escapedChar <|>  noneOf "$[]*+?()|.\\/")
        reAnyChar                 = const RE.dot <$> char '.'

reParens :: Parser (RE Char)
reParens = char '(' *> nonmatch  *> parser <* char ')'
  where nonmatch = optional $ try $ string "?:"

reTerm :: Parser (RE Char)
reTerm = (reParens <|> reChar)

rePostfix :: Parser (RE Char)
rePostfix = f <$> reTerm <*> optionMaybe (oneOf "*+?") <*> optional (char '?')
  where f x Nothing    _  = x
        f x (Just '*') _  = RE.kleene x
        f x (Just '?') _  = x <> RE.eps
        f x (Just '+') _  = x <> RE.kleene x
        f _ (Just x)   _  = error $ "rePostfix: " ++ [x]

append :: Parser (RE Char)
append = foldl RE.append RE.eps <$> many rePostfix

reGroup :: Parser (RE Char)
reGroup = RE.unions <$> sepBy1 append (char '|')

parser :: Parser (RE Char)
parser = reGroup

parseRe :: String -> Either String (RE Char)
parseRe input = case parse (parser <* eof) "" input of
                  Right res -> Right res
                  Left err  -> Left $ show err

unsafeParseRe :: String -> RE Char
unsafeParseRe input = case parse (parser <* eof) "" input of
                        Right res -> res
                        Left err  -> error $ show err
