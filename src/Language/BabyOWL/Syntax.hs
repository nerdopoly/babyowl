module Language.BabyOWL.Syntax where

import Language.BabyOWL.Token (Ident (..))

data Declaration
    = Subclass Ident Class
    | Subrelation Ident Ident
    | Instance Ident
    | Disjoint [Ident]
    | Function Ident
    | Inverse Ident Ident
    | Transitive Ident
    deriving Show

data Class
    = Union Class Class
    | Intersection Class Class
    | RelationExists Ident Class
    | RelationAll Ident Class
    | Complement Class
    | LitThing
    | LitNothing
    | Literal Ident
    deriving Show
