module Text.Tarzan.Regex (
  RE,
  empty,
  nothing,
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
  kstar,
  kplus,
  parser,
  parseRe,
  prettyRe,
  unsafeParseRe,
  escapedChar,
  ) where

import Data.Tarzan
import Text.Tarzan.Regex.Parser
