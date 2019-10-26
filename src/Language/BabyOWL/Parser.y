{
module Language.BabyOWL.Parser (parseTokens) where

import Language.BabyOWL.Token
import Language.BabyOWL.Syntax
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
    '.'             { TokenPeriod }
    '⊑'             { TokenSubclass }
    '≤'             { TokenSubrelation }
    '('             { TokenOpenParen }
    ')'             { TokenCloseParen }
    ','             { TokenComma }
    '⊔'             { TokenUnion }
    '⊓'             { TokenIntersection }
    '∃'             { TokenExists }
    '∀'             { TokenForAll }
    ':'             { TokenColon }
    '¬'             { TokenNot }
    'inst'          { TokenInst }
    'disjoint'      { TokenDisjoint }
    'function'      { TokenFunction }
    'inverse'       { TokenInverse }
    'transitive'    { TokenTransitive }
    '⊤'             { TokenThing }
    '⊥'             { TokenNothing }
    IDENT           { TokenIdent $$ }

%%

declarations :: { [Declaration] }
    : {- empty -}                   { [] }
    | declaration '.' declarations  { $1:$3 }

declaration :: { Declaration }
    : IDENT '⊑' term                    { Subclass $1 $3 }
    | IDENT '≤' IDENT                   { Subrelation $1 $3 }
    | 'inst' '(' IDENT ')'              { Instance $3 }
    | 'disjoint' '(' list ')'           { Disjoint $3 }
    | 'function' '(' IDENT ')'          { Function $3 }
    | 'inverse' '(' IDENT ',' IDENT ')' { Inverse $3 $5 }
    | 'transitive' '(' IDENT ')'        { Transitive $3 }

list :: { [Ident] }
    : IDENT             { [$1] }
    | IDENT ',' list    { $1:$3 }

term :: { Class }
    : union_term    { $1 }

union_term :: { Class }
    : union_term '⊔' intersection_term  { Union $1 $3 }
    | intersection_term                 { $1 }

intersection_term :: { Class }
    : intersection_term '⊓' prefix_term { Intersection $1 $3 }
    | prefix_term                       { $1 }

prefix_term :: { Class }
    : '∃' IDENT ':' prefix_term { RelationExists $2 $4 }
    | '∀' IDENT ':' prefix_term { RelationAll $2 $4 }
    | '¬' prefix_term           { Complement $2 }
    | literal_term              { $1 }

literal_term :: { Class }
    : '(' term ')'  { $2 }
    | IDENT         { Literal $1 }
    | '⊤'           { LitThing }
    | '⊥'           { LitNothing }

{
parseError :: [Token] -> a
parseError _ = error "parse error"
}
