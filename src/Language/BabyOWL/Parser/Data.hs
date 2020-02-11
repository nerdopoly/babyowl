module Language.BabyOWL.Parser.Data
    ( Posn, newPosn
    , PState (..)
    , initPState
    , Parser (..)
    , errorP, getState, setState
    ) where

import Data.Word (Word8)

import Language.BabyOWL.Data (Result)

data Posn = Posn !Int !Int

instance Show Posn where
    show (Posn ln col) = concat ["line ",show ln,", col ",show col]

newPosn :: Char -> Posn -> Posn
newPosn '\n' (Posn ln _) = Posn (ln + 1) 1
newPosn '\r' (Posn ln _) = Posn ln 1
newPosn _ (Posn ln col)  = Posn ln (col + 1)

data PState = PState
    { posn :: !Posn
    , prevChar :: !Char
    , charBuf :: ![Word8]
    , input :: String
    } deriving Show

initPState :: String -> PState
initPState s = PState
    { posn=Posn 1 1
    , prevChar='\n'
    , charBuf=[]
    , input=s
    }

newtype Parser a = P (PState -> (Result a,PState))

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (P xcont) = P $ \st ->
        let (x,st') = xcont st in (fmap f x,st')

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = P $ \st -> (Right x,st)

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (P fcont) <*> (P xcont) = P $ \st ->
        let (f,st') = fcont st
            (x,st'') = xcont st'
        in (f <*> x,st'')

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (P xcont) >>= f = P $ \st ->
        let (r,st') = xcont st
        in case r of
            Right x  -> let (P x'cont) = f x in x'cont st'
            Left err -> (Left err,st')

errorP :: String -> Parser a
errorP err = P $ \st -> (Left err,st)

getState :: Parser PState
getState = P $ \st -> (return st,st)

setState :: PState -> Parser ()
setState st = P $ const (return (),st)
