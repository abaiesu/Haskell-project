{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
module Parser (runParser, parseCmd, parseInput) where

import Cmd
import Data.Maybe
import Data.Char

import Control.Applicative

-- redefining Parser
newtype Parser tok a = Parser { runParser :: [tok] -> Maybe (a,[tok]) }

instance Monad (Parser tok) where
  return :: a -> Parser tok a
  return x = Parser (\ts -> Just (x,ts))

  (>>=) :: Parser tok a -> (a -> Parser tok b) -> Parser tok b
  p >>= f  = Parser (\ts -> case runParser p ts of
                             Nothing -> Nothing
                             Just (x,ts') -> runParser (f x) ts')

-- functor and application
instance Functor (Parser tok) where
  fmap :: (a -> b) -> Parser tok a -> Parser tok b
  fmap f p = p >>= \x -> return (f x)

instance Applicative (Parser tok) where
  pure :: a -> Parser tok a
  pure = return
  (<*>) :: Parser tok (a -> b) -> Parser tok a -> Parser tok b
  pf <*> p = pf >>= \f -> p >>= \x -> return (f x)


-- For backtracking Parser
instance Alternative (Parser tok) where
  empty :: Parser tok a
  empty = Parser (\ts -> Nothing)

  (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
  p1 <|> p2 = Parser (\ts -> case runParser p1 ts of
                               Just (x,ts') -> Just (x,ts')
                               Nothing -> runParser p2 ts)


-- Reads and returns a token
token :: Parser tok tok
token = Parser $ \ts -> case ts of
                          []     -> Nothing
                          (t:ts') -> Just (t,ts')

-- The "sat p" parser matches a token satisfying the predicate p.
sat :: (tok -> Bool) -> Parser tok tok
sat p = do
  t <- token
  if p t then return t else empty

-- matches a string and ignores case
match :: String -> Parser String String
match s = sat (\s' -> map toLower s == map toLower s')

-- We parse English digit words as digits
number :: Parser String Int
number = do
  (match "one" >> return 1)    <|> (match "two" >> return 2) <|>
   (match "three" >> return 3) <|> (match "four" >> return 4) <|>
   (match "five" >> return 5)  <|> (match "six" >> return 6) <|>
   (match "seven" >> return 7) <|> (match "eight" >> return 8) <|>
   (match "nine" >> return 9)

-- parseCmd is our general-purpose parser for command
parseCmd :: Parser String Cmd
parseCmd = parseClimb <|> parseAction <|> parseQuit

-- Parse a climbing or going command.
parseClimb :: Parser String Cmd
parseClimb = do
  match "climb" <|> match "go"
  (match "down" >> return Go_Back) <|>
   (match "left" >> return Go_Left) <|>
   (match "right" >> return Go_Right) <|>
   (match "flag">> return Go_Flag) 

-- Parse an action command
parseAction :: Parser String Cmd
parseAction = do
    match "place"  <|> match "do"
    (match "shoot" >> return Do_Shoot) <|> 
     (match "collect" >> return Do_Collect) <|> 
     (match "feed" >> return Do_Feed) <|>
     (match "flag">> return Place_Flag) 

-- Parse a quit command
parseQuit :: Parser String Cmd
parseQuit = do
  match "quit" <|> match "q"
  return Quit

-- This function runs in any MonadFail monad, to deal with the possiblity of failure.
parseInput :: MonadFail m => Parser String a -> String -> m a
parseInput p s = case runParser p (words s) of
                   Just (x,ts') -> if null ts' then return x else fail "parseInput: some tokens left"
                   Nothing -> fail "parseInput: failed to parse"
