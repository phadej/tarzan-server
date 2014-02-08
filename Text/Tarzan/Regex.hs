module Text.Tarzan.Regex (
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
  parser,
  parseRe,
  unsafeParseRe,
  escapedChar,
  ) where

import Text.Tarzan.Regex.Definitions
import Text.Tarzan.Regex.Parser
