module Text.Tarzan.Pretty where

import Text.Printf

import Data.RangeSet.List (RSet)
import qualified Data.RangeSet.List as RSet

class Pretty a where
  pretty :: a -> String

instance Pretty Char where
  pretty = escapeChar

escapeChar :: Char -> String
escapeChar '\n'   = "\\n"
escapeChar '\t'   = "\\t"
escapeChar '\r'   = "\\r"
escapeChar c
  | ord > 0xffff  = error "escapeChar: out of BMP"
  | ord < 0x20    = '\\' : 'x' : printf "%02x" ord
  | ord >= 0x80   = '\\' : 'u' : printf "%04x" ord
  | c `elem` e    = '\\' : [c]
  | otherwise     = [c]
  where e    = "^$?+[]*()|\\/-."
        ord  = fromEnum c

prettyRSetChar :: RSet Char -> String
prettyRSetChar r
  | RSet.null r   = []
  | dot == r      = "."
  | s == 1        = escapeChar $ head $ RSet.toList r
  | s < m - s     = prettyRSetChar' r
  | otherwise     = prettyRSetChar'' (RSet.complement r)
  where s    = RSet.size r
        m    = fromEnum (maxBound :: Char) - fromEnum (minBound :: Char)
        dot  = RSet.complement $ RSet.singleton '\n'

prettyRSetChar' :: RSet Char -> String
prettyRSetChar' r        = "[" ++ concatMap p l ++ "]"
    where l              = RSet.toRangeList r
          p (a, b)
            | a == b     = escapeChar a
            | otherwise  = escapeChar a ++ "-" ++ escapeChar b

prettyRSetChar'' :: RSet Char -> String
prettyRSetChar'' r       = "[^" ++ concatMap p l ++ "]"
    where l              = RSet.toRangeList r
          p (a, b)
            | a == b     = escapeChar a
            | otherwise  = escapeChar a ++ "-" ++ escapeChar b
