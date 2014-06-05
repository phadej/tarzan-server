module Data.Tarzan (
  -- * Regular expression type
  RE,
  -- * Terminals
  empty,
  nothing,
  anything,
  eps,
  anychar,
  char,
  chars,
  dot,
  string,
  -- * Concatenation
  append,
  (<.>),
  -- * Alternation
  union,
  unions,
  -- * Kleene star & plus
  kleene,
  kstar,
  kplus,
  -- * Matching
  -- | Matching implementation is based on ideas in
  -- <http://dl.acm.org/citation.cfm?id=1520288 Janusz A. Brzozowski: Derivatives of Regular Expressions> and
  -- <http://dl.acm.org/citation.cfm?id=1520288 Scott Owens, John Reppy, Aaron Turon: Regular-expression derivatives re-examined>
  nullable,
  derivate,
  matches,
  (~=),
  -- * Equality
  eq,
  ne,
  -- * Pretty printing
  prettyRe,
  -- * Utility
  leadingCSets
) where

-- http://r6.ca/blog/20110808T035622Z.html

import Prelude hiding (negate, any, all)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.RangeSet.List (RSet)
import qualified Data.RangeSet.List as RSet

import Data.Monoid
import Data.List (intercalate, foldl')
import Data.Foldable (any, all)
import Data.Either

import Text.Printf

import Control.Monad.Trans.State

{-
import Debug.Trace
debug :: Show a => a -> a
debug x = traceShow x x
-}

data RE a = REChars (RSet a)
          | REAppend [RE a]
          | REUnion (Set (RE a))
          | REKleene (RE a)
  deriving (Eq, Ord, Show)

instance (Ord a, Enum a) => Monoid (RE a) where
	mempty = empty
	mappend = union
	mconcat = unions

empty :: RE a
empty = REChars RSet.empty

nothing :: RE a
nothing = empty

anything :: (Ord a, Enum a, Bounded a) => RE a
anything = REKleene anychar

eps :: RE a
eps = REKleene nothing

char :: a -> RE a
char = REChars . RSet.singleton

chars :: (Ord a, Enum a, Bounded a) => Bool -> [(a,a)] -> RE a
chars pos cs = REChars . con $ s
  where con | pos             = id
            | otherwise       = RSet.complement
        s                     = mconcat $ map RSet.singletonRange cs

dotSet :: RSet Char
dotSet = RSet.complement $ RSet.singleton '\n'

dot :: RE Char
dot = REChars dotSet

string :: Ord a => [a] -> RE a
string = foldr append eps . map char

anychar :: Bounded a => RE a
anychar = REChars RSet.full

appends :: Ord a => [RE a] -> RE a
appends rs
  | any (== empty) rs  = empty
  | otherwise          = case rs' of
                           []   -> eps
                           [r]  -> r
                           _    -> REAppend rs'
  where rs' = filter (/= eps) rs

append :: Ord a => RE a-> RE a -> RE a
append a b = appends [a, b]

(<.>) :: Ord a => RE a -> RE a -> RE a
(<.>) = append

union :: (Ord a, Enum a) => RE a -> RE a -> RE a
union a b = unions [a, b]

extractCharacterSets :: RE a -> Either (RSet a) (RE a)
extractCharacterSets (REChars c)  = Left c
extractCharacterSets r            = Right r

unions :: (Ord a, Enum a) => [RE a] -> RE a
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

kleene :: (Ord a, Enum a, Bounded a) => RE a -> RE a
kleene r
  | r == empty       = eps
  | r == anything    = anything -- this and following are special cases of (REKleene r) case
  | r == eps         = eps
kleene (REKleene r)  = REKleene r
kleene r             = REKleene r

kstar :: (Ord a, Enum a, Bounded a) => RE a -> RE a
kstar = kleene

kplus :: (Ord a, Enum a, Bounded a) => RE a -> RE a
kplus r = r <.> kstar r

-- matching

-- http://dl.acm.org/citation.cfm?id=1520288

nullable :: RE a -> Bool
nullable (REChars _)     = False
nullable (REAppend rs)   = all nullable rs
nullable (REUnion rs)    = any nullable rs
nullable (REKleene _)    = True

derivateAppend :: (Ord a, Enum a) => a -> [RE a] -> RE a
derivateAppend _ []      = nothing
derivateAppend c [r]     = derivate c r
derivateAppend c (r:rs)
  | nullable r           = r' <.> appends rs <> rs'
  | otherwise            = r' <.> appends rs
  where r'  = derivate c r
        rs' = derivateAppend c rs

derivate :: (Ord a, Enum a) => a -> RE a -> RE a
derivate c (REChars cs)
  | c `RSet.member` cs      = eps
  | otherwise               = nothing
derivate c (REUnion rs)     = unions (map (derivate c) $ Set.toList rs)
derivate c (REAppend rs)    = derivateAppend c rs
derivate c rs@(REKleene r)  = derivate c r <.> rs

matches :: (Ord a, Enum a) => RE a -> [a] -> Bool
matches r = nullable . foldl' (flip derivate) r

infix 4 ~=

-- | Flipped infix version of 'matches'.
(~=) :: (Ord a, Enum a) => [a] -> RE a -> Bool
(~=) = flip matches

-- character sets

wedge :: (Ord a, Enum a) => [RSet a] -> [RSet a] -> [RSet a]
wedge as bs = sortUniq $ filter (not . RSet.null) [ a `RSet.intersection` b | a <- as, b <- bs ]

leadingCSetsAppend :: (Ord a, Enum a, Bounded a) => [RE a] -> [RSet a]
leadingCSetsAppend []      = [RSet.full]
leadingCSetsAppend [r]     = leadingCSets r
leadingCSetsAppend (r:rs)
  | nullable r             = leadingCSets r `wedge` leadingCSetsAppend rs
  | otherwise              = leadingCSets r

leadingCSets :: (Ord a, Enum a, Bounded a) => RE a -> [RSet a]
leadingCSets (REChars r)    = [r, RSet.complement r]
leadingCSets (REUnion rs)   = foldl' wedge [RSet.full] $ map leadingCSets $ Set.toList rs
leadingCSets (REAppend rs)  = leadingCSetsAppend rs
leadingCSets (REKleene r)   = leadingCSets r

-- equality

derivatePair :: (Ord a, Enum a) => a -> (RE a, RE a) -> (RE a, RE a)
derivatePair c (a, b) = (derivate c a, derivate c b)

infix 4 `eq`
infix 4 `ne`

eq :: (Show a, Ord a, Enum a, Bounded a) => RE a -> RE a -> Bool
eq r s = evalState (eqState (r,s)) Set.empty

ne :: (Show a, Ord a, Enum a, Bounded a) => RE a -> RE a -> Bool
ne r s = not (eq r s)

eqState :: (Show a, Ord a, Enum a, Bounded a) => (RE a, RE a) -> State (Set (RE a, RE a)) Bool
eqState p@(a, b) = do
  s <- get
  let deriv' = deriv `Set.difference` s
  if all nullp $ deriv'
    then do
      put (Set.insert p s)
      and `fmap` mapM eqState (Set.toList deriv')
    else return False  
  where csets = leadingCSets a `wedge` leadingCSets b
        chars = map RSet.findMin csets
        deriv = Set.fromList $ map (flip derivatePair p) chars
        nullp (a, b) = nullable a == nullable b

-- pretty 

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
  | dotSet == r   = "."
  | s == 1        = escapeChar $ head $ RSet.toList r
  | s < m - s     = prettyRSetChar' r
  | otherwise     = prettyRSetChar'' (RSet.complement r)
  where s    = RSet.size r
        m    = fromEnum (maxBound :: Char) - fromEnum (minBound :: Char)

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

prettyRe :: RE Char -> String
prettyRe r | r == eps       = ""
prettyRe r | r == nothing   = "[]"
prettyRe r | r == anything  = "[^]*"
prettyRe (REChars cs)       = prettyRSetChar cs
prettyRe (REAppend rs)      = concatMap prettyRe rs
prettyRe (REUnion rs)       = "(?:" ++ intercalate "|" rs' ++ ")" ++ opt
  where rs' = map prettyRe $ Set.toList $ Set.delete eps rs
        opt | eps `Set.member` rs = "?"
            | otherwise           = ""
prettyRe (REKleene r)       = case prettyRe r of
                                [c]  -> c : "*"
                                str  -> "(" ++ str ++ ")*"

-- Utilities

sortUniq :: Ord a => [a] -> [a]
sortUniq = Set.toList . Set.fromList
