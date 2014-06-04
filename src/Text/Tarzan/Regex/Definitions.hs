module Text.Tarzan.Regex.Definitions (
  RE,
  empty,
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
  nullable,
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

import Text.Tarzan.Pretty



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

data RE = REEps
        | REChars (RSet Char)
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
  pretty (REChars cs)    = prettyRSetChar cs
  pretty (REAppend a b)  = pretty a ++ pretty b
  pretty (REUnion rs)    = "(?:" ++ intercalate "|" rs' ++ ")" ++ opt
    where rs' = map pretty $ Set.toList $ Set.delete eps rs
          opt | eps `Set.member` rs = "?"
              | otherwise           = ""
  pretty (REKleene r)    = case pretty r of
                             [c]           -> c : "*"
                             str           -> "(" ++ str ++ ")*"

empty :: RE
empty = REChars RSet.empty

anything :: RE
anything = REKleene anychar

eps :: RE
eps = REEps

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

extractCharacterSets :: RE -> Either (RSet Char) RE
extractCharacterSets (REChars c)  = Left c
extractCharacterSets r            = Right r

sortUniq :: Ord a => [a] -> [a]
sortUniq = Set.toList . Set.fromList

nullable :: RE -> Bool
nullable REEps           = True
nullable (REChars _)     = False
nullable (REAppend a b)  = nullable a && nullable b
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
  | r == empty       = REEps
  | r == anything    = anything
  | r == eps         = REEps
kleene (REKleene r)  = REKleene r
kleene r             = REKleene r
