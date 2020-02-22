module Main (main) where

import System.Exit (die)

import Language.BabyOWL.Parser (parse)
import Language.BabyOWL.Semantics (check)

main :: IO ()
main = do
    input  <- getContents
    case parse input of
        Right ast -> do
            putStrLn $ "AST: " ++ show ast
            case check ast of
                Right syms -> putStrLn $ "Symbols: " ++ show syms
                Left err    -> die $ "type error: " ++ err
        Left err  -> die err
