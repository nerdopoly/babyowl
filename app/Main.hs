module Main (main) where

import System.Exit (die)

import Language.BabyOWL.Parser (parseString)
import Language.BabyOWL.Semantics (check)

main :: IO ()
main = do
    input  <- getContents
    case parseString input of
        Right ast -> do
            print ast
            case check ast of
                Right syms -> print syms
                Left err    -> die $ "type error: " ++ err
        Left err  -> die err
