module Main (main) where

import System.Exit (die)

import Language.BabyOWL.Parser (parse)
import Language.BabyOWL.Semantics (check)

main :: IO ()
main = do
    txt <- getContents
    let ast = parse txt
    print ast
    case check ast of
        Right syms -> print syms
        Left err   -> die $ "type error: " ++ err