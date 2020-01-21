module Language.BabyOWL.Semantics (check) where

import qualified Data.Map.Strict as Map

import Language.BabyOWL.Data (Ident (Ident), Result)
import Language.BabyOWL.Syntax (Declaration (..), ClassExp (..))

data Type = Class | Relation deriving (Show,Eq)
type Context = Map.Map Ident Type

type TypeCheck = Context -> Result Context

assert :: Ident -> Type -> TypeCheck
assert i@(Ident name) t g =
    case Map.lookup i g of
        Just t' -> if t == t'
                   then return g
                   else Left $ name ++ " is already used as " ++ show t
        Nothing -> return $ Map.insert i t g

check :: [Declaration] -> Result Context
check = flip go Map.empty
    where
        go :: [Declaration] -> TypeCheck
        go (d:ds) g = checkDecl d g >>= go ds
        go [] g     = return g

checkDecl :: Declaration -> TypeCheck
checkDecl (Subclass c e) g    = assert c Class g >>= checkExp e
checkDecl (Subrelation r s) g = assert r Relation g >>= assert s Relation
checkDecl (Instance c) g      = assert c Class g
checkDecl (Disjoint (c:cs)) g = assert c Class g >>= checkDecl (Disjoint cs)
checkDecl (Disjoint []) g     = return g
checkDecl (Function r) g      = assert r Relation g
checkDecl (Inverse r s) g     = assert r Relation g >>= assert s Relation
checkDecl (Transitive r) g    = assert r Relation g

checkExp :: ClassExp -> TypeCheck
checkExp (Union l r) g          = checkExp l g >>= checkExp r
checkExp (Intersection l r) g   = checkExp l g >>= checkExp r
checkExp (RelationExists i e) g = assert i Relation g >>= checkExp e
checkExp (RelationAll i e) g    = assert i Relation g >>= checkExp e
checkExp (Complement e) g       = checkExp e g
checkExp LitThing g             = return g
checkExp LitNothing g           = return g
checkExp (Literal i) g          = assert i Class g
