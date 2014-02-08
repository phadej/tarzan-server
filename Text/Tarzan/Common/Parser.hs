module Text.Tarzan.Common.Parser where

import Control.Applicative hiding (many, (<|>), optional)

import Text.Parsec
import Text.Parsec.String

import Data.Char

escapedChar :: Parser Char
escapedChar = char '\\' *> (x <|> u <|> n <|> r <|> t <|> anyChar)
  where n   = char 'n' *> return '\n'
        r   = char 'r' *> return '\r'
        t   = char 't' *> return '\t'
        x   = x' <$> char 'x' <*> hexDigit <*> hexDigit
        x' _ a b = toEnum $ 0x10 * (digitToInt a) + (digitToInt b)
        u   = u' <$> char 'u' <*> hexDigit <*> hexDigit <*> hexDigit <*> hexDigit
        u' _ a b c d = toEnum $ 0x1000 * (digitToInt a) + 0x100 * (digitToInt b) + 0x10 * (digitToInt c) + (digitToInt d)

ws :: Parser ()
ws = skipMany (oneOf " \r\t")

lexeme :: Parser a -> Parser a
lexeme = (<* ws)

identifier :: Parser String
identifier = lexeme ((:) <$> starting <*> many rest)
  where starting  = satisfy (\c -> isAlpha c || c == '_')
        rest      = satisfy (\c -> isAlphaNum c || c == '_')
