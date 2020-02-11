{
module Language.BabyOWL.Parser.Lexer (lexer) where

import Data.Word (Word8)
import Codec.Binary.UTF8.String (encodeChar)

import Language.BabyOWL.Data (Ident (Ident))

import Language.BabyOWL.Parser.Token (Token (..))
import Language.BabyOWL.Parser.Data
    ( newPosn
    , PState(..)
    , Parser
    , errorP, getState, setState
    )
}

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

{
type AlexInput = PState

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte PState{charBuf=[],input=""}              = Nothing
alexGetByte st@PState{charBuf=(b:bs)}                = Just (b,st{charBuf=bs})
alexGetByte st@PState{posn=p,charBuf=[],input=(c:s)} = Just (b,st')
    where
        st' = st{posn=p',prevChar=c,charBuf=bs,input=s}
        (b:bs) = encodeChar c
        p' = newPosn c p

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = prevChar

lexer :: (Token -> Parser a) -> Parser a
lexer cont = do
    st <- getState
    case alexScan st 0 of
        AlexEOF             -> cont TokenEOF
        AlexError _         -> errorP "lexical error"
        AlexSkip st' _      -> setState st' >> lexer cont
        AlexToken st' l act -> do
            let t = act . take l . input $ st
            setState st'
            cont t
}
