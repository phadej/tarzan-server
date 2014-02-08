module Text.Tarzan.Regex.Definitions (
  RE,
  empty,
  anything,
  eps,
  char,
  chars,
  string,
  anychar,
  append,
  (<.>),
  union,
  unions,
  kleene,
) where

import Prelude hiding (negate)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.RangeSet.List (RSet)
import qualified Data.RangeSet.List as RSet

import Data.Monoid
import Data.List (intercalate)
import Data.Either

import Text.Printf

import Text.Tarzan.Pretty

data REChar = SingleChar Char
            | CharRange Char Char

escapeChar :: Char -> String
escapeChar '\n' = "\\n"
escapeChar '\t' = "\\t"
escapeChar '\r' = "\\r"
escapeChar c
  | ord > 0xffff  = error "out of bmp"
  | ord < 0x20    = '\\' : 'x' : printf "%02x" ord
  | ord >= 0x80   = '\\' : 'u' : printf "%04x" ord
  | c `elem` e    = '\\' : [c]
  | otherwise     = [c]
  where e    = "^$?+[]*()|\\/-."
        ord  = fromEnum c

instance Show REChar where
  show (SingleChar c)   = escapeChar c
  show (CharRange a b)
    | succ a == b       = escapeChar a ++ escapeChar b
    | otherwise         = escapeChar a ++ "-" ++ escapeChar b

fromRange :: (Char, Char) -> REChar
fromRange (a, b) | a == b     = SingleChar a
                 | otherwise  = CharRange a b

data CharacterSet = PositiveSet (RSet Char)
                  | NegativeSet (RSet Char)
  deriving (Eq, Ord, Show)

csempty :: CharacterSet
csempty = PositiveSet RSet.empty

csunion :: CharacterSet -> CharacterSet -> CharacterSet
csunion (PositiveSet a) (PositiveSet b) = PositiveSet $ a <> b
csunion (NegativeSet a) (NegativeSet b) = NegativeSet $ a `RSet.intersection` b
csunion (PositiveSet a) (NegativeSet b) = NegativeSet $ b `RSet.difference` a
csunion (NegativeSet a) (PositiveSet b) = NegativeSet $ a `RSet.difference` b

instance Pretty CharacterSet where
  pretty (PositiveSet cs) = show' $ RSet.toRangeList cs
    where show' [(a, b)] | a == b = escapeChar a
          show' xs                = "[" ++ concatMap (show . fromRange) xs ++ "]"
  pretty (NegativeSet cs)
   | RSet.null cs    = "."
   | otherwise       = show' $ RSet.toRangeList cs
    where show' xs   = "[^" ++ concatMap (show . fromRange) xs ++ "]"

instance Monoid CharacterSet where
  mempty = csempty
  mappend = csunion

data RE = REEps
        | REChars CharacterSet
        | REAppend RE RE
        | REUnion (Set RE)
        | REKleene RE
  deriving (Eq, Ord, Show)

instance Monoid RE where
	mempty = empty
	mappend = union
	mconcat = unions

instance Pretty RE where
  pretty REEps           = ""
  pretty (REChars cs)    = pretty cs
  pretty (REAppend a b)  = pretty a ++ pretty b
  pretty (REUnion rs)    = "(?:" ++ intercalate "|" rs' ++ ")" ++ opt
    where rs' = map pretty $ Set.toList $ Set.delete eps rs
          opt | eps `Set.member` rs = "?"
              | otherwise           = ""
  pretty (REKleene r)    = case pretty r of
                             [c]           -> c : "*"
                             str           -> "(" ++ str ++ ")*"

empty :: RE
empty = REChars csempty

anything :: RE
anything = REKleene anychar

eps :: RE
eps = REEps

char :: Char -> RE
char = REChars . PositiveSet . RSet.singleton

chars :: Bool -> [(Char,Char)] -> RE
chars pos cs = REChars . con $ s
  where con | pos          = PositiveSet
            | otherwise    = NegativeSet
        s                  = mconcat $ map (f . fromRange)   cs
        f (SingleChar c)   = RSet.singleton c
        f (CharRange a b)  = RSet.singletonRange (a, b)

string :: String -> RE
string = foldr append eps . map char

anychar :: RE
anychar = REChars . NegativeSet $ RSet.empty

append :: RE -> RE -> RE
append (REAppend a b) c       = append a (append b c)
append a b
  | a == empty || b == empty  = empty
  | a == eps                  = b
  | b == eps                  = a
  | otherwise                 = REAppend a b

(<.>) :: RE -> RE -> RE
(<.>) = append

union :: RE -> RE -> RE
union a b = unions [a, b]

extractCharacterSets :: RE -> Either CharacterSet RE
extractCharacterSets (REChars c) = Left c
extractCharacterSets r          = Right r

sortUniq :: Ord a => [a] -> [a]
sortUniq = Set.toList . Set.fromList

unions :: [RE] -> RE
unions = con . split . flatten . sortUniq
  where flatten = concatMap extract
        extract (REUnion xs) = Set.toList xs
        extract x            = [x]
        split rs  = case partitionEithers $ map extractCharacterSets rs of
                      (cs, rs') -> if cs' == empty then rs' else cs' : rs'
                                      where cs' = REChars (mconcat cs)
        con []   = empty
        con [r]  = r
        con rs   = REUnion . Set.fromList $ rs

kleene :: RE -> RE
kleene r
  | r == empty       = REEps
  | r == anything    = anything
  | r == eps         = REEps
kleene (REKleene r)  = REKleene r
kleene r             = REKleene r
