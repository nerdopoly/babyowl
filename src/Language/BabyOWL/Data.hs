module Language.BabyOWL.Data
    ( Ident (Ident)
    , Result
    ) where

newtype Ident = Ident String deriving (Eq,Ord)

instance Show Ident where
    show (Ident s) = '$':s

type Result a = Either String a
