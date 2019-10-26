module Language.BabyOWL (parse) where

import Language.BabyOWL.Syntax
import Language.BabyOWL.Lexer (alexScanTokens)
import Language.BabyOWL.Parser (parseTokens)

parse :: String -> [Declaration]
parse = parseTokens . alexScanTokens
