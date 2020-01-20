{
module Language.BabyOWL.Parser.Lexer (alexScanTokens) where

import Language.BabyOWL.Data (Ident (Ident))

import Language.BabyOWL.Parser.Token
}

%wrapper "basic"

babyowl :-
    $white+         ;
    \.              { \_ -> TokenPeriod }
    ⊑|::            { \_ -> TokenSubclass }
    ≤|\<:           { \_ -> TokenSubrelation }
    \(              { \_ -> TokenOpenParen }
    \)              { \_ -> TokenCloseParen }
    \,              { \_ -> TokenComma }
    ⊔|\|            { \_ -> TokenUnion }
    ⊓|&             { \_ -> TokenIntersection }
    ∃|£             { \_ -> TokenExists }
    ∀|@             { \_ -> TokenForAll }
    :               { \_ -> TokenColon }
    ¬               { \_ -> TokenNot }
    inst            { \_ -> TokenInst }
    disjoint        { \_ -> TokenDisjoint }
    function        { \_ -> TokenFunction }
    inverse         { \_ -> TokenInverse }
    transitive      { \_ -> TokenTransitive }
    ⊤|Thing         { \_ -> TokenThing }
    ⊥|Nothing       { \_ -> TokenNothing }
    [A-Za-z]+       { TokenIdent . Ident }
