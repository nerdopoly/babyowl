module Language.BabyOWL.Parser.Token (Token (..)) where

import Language.BabyOWL.Data (Ident)

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
