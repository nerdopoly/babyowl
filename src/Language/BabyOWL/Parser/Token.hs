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
    | TokenEOF
    deriving Eq

instance Show Token where
    show TokenPeriod       = "'.'"
    show TokenSubclass     = "'⊑' or '::'"
    show TokenSubrelation  = "'≤' or '<:'"
    show TokenOpenParen    = "'('"
    show TokenCloseParen   = "')'"
    show TokenComma        = "','"
    show TokenUnion        = "'⊔' or '|'"
    show TokenIntersection = "'⊓' or '&'"
    show TokenExists       = "'∃' or '£'"
    show TokenForAll       = "'∀' or '@'"
    show TokenColon        = "':'"
    show TokenNot          = "'¬'"
    show TokenInst         = "'inst'"
    show TokenDisjoint     = "'disjoint'"
    show TokenFunction     = "'function'"
    show TokenInverse      = "'inverse'"
    show TokenTransitive   = "'transitive'"
    show TokenThing        = "'⊤' or 'Thing'"
    show TokenNothing      = "'⊥' or 'Nothing'"
    show (TokenIdent _)    = "identifier"
    show TokenEOF          = "end of file"
