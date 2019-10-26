{
module Language.BabyOWL.Lexer (alexScanTokens) where

import Language.BabyOWL.Token
}

%wrapper "basic"

babyowl :-
    $white+         ;
    \.              { \s -> TokenPeriod }
    ⊑               { \s -> TokenSubclass }
    ≤               { \s -> TokenSubrelation }
    \(              { \s -> TokenOpenParen }
    \)              { \s -> TokenCloseParen }
    \,              { \s -> TokenComma }
    ⊔               { \s -> TokenUnion }
    ⊓               { \s -> TokenIntersection }
    ∃               { \s -> TokenExists }
    ∀               { \s -> TokenForAll }
    :               { \s -> TokenColon }
    ¬               { \s -> TokenNot }
    inst            { \s -> TokenInst }
    disjoint        { \s -> TokenDisjoint }
    function        { \s -> TokenFunction }
    inverse         { \s -> TokenInverse }
    transitive      { \s -> TokenTransitive }
    ⊤               { \s -> TokenThing }
    ⊥               { \s -> TokenNothing }
    [A-Za-z]+       { TokenIdent . Ident }
