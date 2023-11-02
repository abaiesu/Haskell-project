{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Parser (runParser, parseCmd, parseInput) where

import Cmd ( Cmd(..) )
import Data.Maybe ()
import Data.Char ( toLower )

import Control.Applicative ( Alternative((<|>), empty) )

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
  empty = Parser (const Nothing)

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
number = (match "one" >> return 1)    <|> (match "two" >> return 2) <|>
 (match "three" >> return 3) <|> (match "four" >> return 4) <|>
 (match "five" >> return 5)  <|> (match "six" >> return 6) <|>
 (match "seven" >> return 7) <|> (match "eight" >> return 8) <|>
 (match "nine" >> return 9)

-- parseCmd is our general-purpose parser for command
parseCmd :: Parser String Cmd
parseCmd = parseClimb <|> parseAction <|> parseQuit

-- Parse a climbing or going command.
-- Parse a climbing or going command.
-- Parse a climbing or going command.
-- Parse a climbing or going command.
parseClimb :: Parser String Cmd
parseClimb = do
    token1 <- match "go" <|> return "go"
    token2 <- sat isDirection
    return $ case map toLower token2 of
        "left" -> Go_Left
        "right" -> Go_Right
        "back" -> Go_Back
  where
    isDirection :: String -> Bool
    isDirection s = map toLower s `elem` ["left", "right", "back"]



-- Parse an action command
-- Parse an action command
parseAction :: Parser String Cmd
parseAction = do
    token1 <- match "place" <|> match "do" <|> return ""
    token2 <- sat isAction
    return $ case map toLower token2 of
        "shoot" -> Do_Shoot
        "collect" -> Do_Collect
        _ -> Place_Flag -- Default to Do_Feed for single-word commands
  where
    isAction :: String -> Bool
    isAction s = map toLower s `elem` ["shoot", "collect"]


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

