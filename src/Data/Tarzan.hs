module Data.Tarzan (
  RE,
  empty,
  nothing,
  anything,
  eps,
  char,
  chars,
  dot,
  string,
  anychar,
  append,
  (<.>),
  union,
  unions,
  kleene,
  kstar,
  kplus,
  nullable,
  prettyRe,
) where

-- http://r6.ca/blog/20110808T035622Z.html

import Prelude hiding (negate, any, all)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.RangeSet.List (RSet)
import qualified Data.RangeSet.List as RSet

import Data.Monoid
import Data.List (intercalate)
import Data.Foldable (any, all)
import Data.Either

import Text.Printf

data RE = REChars (RSet Char)
        | REAppend [RE]
        | REUnion (Set RE)
        | REKleene RE
  deriving (Eq, Ord, Show)

instance Monoid RE where
	mempty = empty
	mappend = union
	mconcat = unions

empty :: RE
empty = REChars RSet.empty

nothing :: RE
nothing = empty

anything :: RE
anything = REKleene anychar

eps :: RE
eps = REKleene nothing

char :: Char -> RE
char = REChars . RSet.singleton

chars :: Bool -> [(Char,Char)] -> RE
chars pos cs = REChars . con $ s
  where con | pos             = id
            | otherwise       = RSet.complement
        s                     = mconcat $ map RSet.singletonRange cs

dot :: RE
dot = REChars $ RSet.complement $ RSet.singleton '\n'

string :: String -> RE
string = foldr append eps . map char

anychar :: RE
anychar = REChars RSet.full

appends :: [RE] -> RE
appends rs
  | any (== empty) rs  = empty
  | otherwise          = case rs' of
                           []   -> eps
                           [r]  -> r
                           _    -> REAppend rs'
  where rs' = filter (/= eps) rs

append :: RE -> RE -> RE
append a b = appends [a, b]

(<.>) :: RE -> RE -> RE
(<.>) = append

union :: RE -> RE -> RE
union a b = unions [a, b]

extractCharacterSets :: RE -> Either (RSet Char) RE
extractCharacterSets (REChars c)  = Left c
extractCharacterSets r            = Right r

sortUniq :: Ord a => [a] -> [a]
sortUniq = Set.toList . Set.fromList

nullable :: RE -> Bool
nullable (REChars _)     = False
nullable (REAppend rs)   = all nullable rs
nullable (REUnion rs)    = any nullable rs
nullable (REKleene _)    = True

unions :: [RE] -> RE
unions = unions' . split . flatten . sortUniq
  where flatten = concatMap extract
        extract (REUnion xs) = Set.toList xs
        extract x            = [x]
        split rs  = case partitionEithers $ map extractCharacterSets rs of
                      (cs, rs') -> if cs' == empty then rs'' else cs' : rs''
                                      where cs'   = REChars (mconcat cs)
                                            rs''  = rs'
        unions' []   = empty
        unions' [r]  = r
        unions' rs   = REUnion . Set.fromList $ rs

kleene :: RE -> RE
kleene r
  | r == empty       = eps
  | r == anything    = anything -- this and following are special cases of (REKleene r) case
  | r == eps         = eps
kleene (REKleene r)  = REKleene r
kleene r             = REKleene r

kstar :: RE -> RE
kstar = kleene

kplus :: RE -> RE
kplus r = r <.> kstar r

--- pretty 

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

prettyRe :: RE -> String
prettyRe r | r == eps    = ""
prettyRe (REChars cs)    = prettyRSetChar cs
prettyRe (REAppend rs)   = concatMap prettyRe rs
prettyRe (REUnion rs)    = "(?:" ++ intercalate "|" rs' ++ ")" ++ opt
  where rs' = map prettyRe $ Set.toList $ Set.delete eps rs
        opt | eps `Set.member` rs = "?"
            | otherwise           = ""
prettyRe (REKleene r)    = case prettyRe r of
                           [c]           -> c : "*"
                           str           -> "(" ++ str ++ ")*"
