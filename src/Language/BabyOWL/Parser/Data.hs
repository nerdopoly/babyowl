module Language.BabyOWL.Parser.Data
    ( Posn (..)
    , newPosn
    , PState (..)
    , initPState
    , P (..)
    , runP, errorP, getState, setState
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
    , remInput :: String
    } deriving Show

initPState :: String -> PState
initPState s = PState
    { posn=Posn 1 1
    , prevChar='\n'
    , charBuf=[]
    , remInput=s
    }

type PResult a = Result (a,PState)

newtype P a = P (PState -> PResult a)

runP :: P a -> PState -> PResult a
runP (P cont) = cont

instance Functor P where
    -- fmap :: (a -> b) -> P a -> P b
    fmap f p = P $ \st ->
        case runP p st of
            Left err      -> Left err
            Right (x,st') -> Right (f x,st')

instance Applicative P where
    -- pure :: a -> P a
    pure x = P $ \st -> Right (x,st)

    -- (<*>) :: P (a -> b) -> P a -> P b
    fp <*> xp = P $ \st ->
        case runP fp st of
            Left err      -> Left err
            Right (f,st') -> runP (fmap f xp) st'

instance Monad P where
    -- (>>=) :: P a -> (a -> P b) -> P b
    p >>= f = P $ \st ->
        case runP p st of
            Left err      -> Left err
            Right (x,st') -> runP (f x) st'

errorP :: String -> P a
errorP s = do
    PState{posn=p} <- getState
    P . const . Left . concat $ [s," at ",show p]

getState :: P PState
getState = P $ \st -> Right (st,st)

setState :: PState -> P ()
setState st = P . const . Right $ ((),st)
