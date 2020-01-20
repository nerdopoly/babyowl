module Language.BabyOWL.Syntax where

import Language.BabyOWL.Data (Ident)

data Declaration
    = Subclass Ident ClassExp
    | Subrelation Ident Ident
    | Instance Ident
    | Disjoint [Ident]
    | Function Ident
    | Inverse Ident Ident
    | Transitive Ident
    deriving Show

data ClassExp
    = Union ClassExp ClassExp
    | Intersection ClassExp ClassExp
    | RelationExists Ident ClassExp
    | RelationAll Ident ClassExp
    | Complement ClassExp
    | LitThing
    | LitNothing
    | Literal Ident
    deriving Show
