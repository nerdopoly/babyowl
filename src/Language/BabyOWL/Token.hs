module Language.BabyOWL.Token (Ident (..), Token (..)) where

newtype Ident = Ident String deriving (Eq)

instance Show Ident where
    show (Ident s) = '$':s

data Token
    = TokenPeriod
    | TokenSubclass
    | TokenSubrelation
    | TokenOpenParen
    | TokenCloseParen
    | TokenComma
    | TokenUnion
    | TokenIntersection
    | TokenExists
    | TokenForAll
    | TokenColon
    | TokenNot
    | TokenInst
    | TokenDisjoint
    | TokenFunction
    | TokenInverse
    | TokenTransitive
    | TokenThing
    | TokenNothing
    | TokenIdent Ident
    deriving (Show,Eq)
