module Text.Tarzan.Pretty where

class Pretty a where
  pretty :: a -> String
