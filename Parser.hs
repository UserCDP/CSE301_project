module Parser (runParser, parseCmd, parseInput) where

import GameData

import Data.Maybe
import Data.Char
import Items
import Control.Applicative

newtype Parser tok a = Parser { runParser :: [tok] -> Maybe (a,[tok]) }

instance Monad (Parser tok) where
  return x = Parser (\ts -> Just (x,ts))
  p >>= f  = Parser (\ts -> case runParser p ts of
                             Nothing -> Nothing
                             Just (x,ts') -> runParser (f x) ts')

instance Functor (Parser tok) where
  fmap f p = p >>= \x -> return (f x)

instance Applicative (Parser tok) where
  pure = return
  pf <*> p = pf >>= \f -> p >>= \x -> return (f x)

instance Alternative (Parser tok) where
  empty = Parser (\ts -> Nothing)
  p1 <|> p2 = Parser (\ts -> case runParser p1 ts of
                               Just (x,ts') -> Just (x,ts')
                               Nothing -> runParser p2 ts)

token :: Parser tok tok
token = Parser $ \ts -> case ts of
                          []     -> Nothing
                          (t:ts') -> Just (t,ts')

sat :: (tok -> Bool) -> Parser tok tok
sat p = do
  t <- token
  if p t then return t else empty

match :: String -> Parser String String
match s = sat (\s' -> map toLower s == map toLower s')

number :: Parser String Int
number = do
  (match "one" <|> match "1" >> return 1)    <|> (match "two" <|> match "2" >> return 2) <|>
   (match "three" <|> match "3" >> return 3) <|> (match "four" <|> match "4" >> return 4) <|>
   (match "five" <|> match"5" >> return 5)  <|> (match "six" <|> match "6" >> return 6) <|>
   (match "seven" <|> match "7" >> return 7) <|> (match "eight" <|> match "8" >> return 8) <|>
   (match "nine" <|> match "9" >> return 9) <|> (match "zero" <|> match "0" >> return 0)

-- parseCmd is our general-purpose parser for commands

parseCmd :: Parser String Cmd
parseCmd = parseClimb  <|> parseQuit <|> parseShowOxygen <|> 
           parseHelp <|> parseUseItem <|> parseGetItem  <|> parseCheckInventory


-- Parse a moving command.
parseClimb :: Parser String Cmd
parseClimb = do
  match "climb" <|> match "go" <|> match "move"
  (match "up" <|> match "back" >> return Go_Up) <|>
   (match "left" >> return Go_Left) <|>
   (match "right" >> return Go_Right)

---- Parse an item related command, which is either Use item or Get Item

parseUseKey :: Parser String Cmd
parseUseKey = do
  match "use" <|> match "try"
  match "key"
  n <- number
  return (Use (Key n))

parseUseMap :: Parser String Cmd
parseUseMap = do
  match "check"
  match "map"
  n <- number
  return (Use (Map n))

parseUseOxygenTank :: Parser String Cmd
parseUseOxygenTank = do
  match "use"
  match "oxygen" <|> match "o"
  n <- number
  return (Use (OxygenTank n))


--- Parse a general use item command 
parseUseItem :: Parser String Cmd
parseUseItem = do
  parseUseKey <|> (match "use" >> match "shovel" >> return (Use Shovel)) <|> parseUseMap <|> parseUseOxygenTank


-- Parse a general get item command
parseGetItem :: Parser String Cmd 
parseGetItem = do
  match "get" <|> match "pick up" <|> match "pickup"
  match "key" <|> match "item" <|> match "shovel"
  return PickUp



--- Parse commands which informs the player of his current state


parseCheckInventory :: Parser String Cmd 
parseCheckInventory = do
  (match "check" >> match "inventory" >> return Check_Inventory) <|> (match "i" >> return Check_Inventory)


parseHelp :: Parser String Cmd
parseHelp = do
  match "help" <|> match "h"
  return Help


parseShowOxygen :: Parser String Cmd
parseShowOxygen = do
  match "o" <|> match "oxygen"
  return Show_Oxygen


---- Parse a quit game command

parseQuit :: Parser String Cmd
parseQuit = do
  match "quit" <|> match "q"
  return Quit


-- Finally, we export a function that runs a parser on the entire input string, broken up into words.
-- This function runs in any MonadFail monad, to deal with the possiblity of failure.
parseInput :: MonadFail m => Parser String a -> String -> m a
parseInput p s = case runParser p (words s) of
                   Just (x,ts') -> if null ts' then return x else fail "parseInput: some tokens left"
                   Nothing -> fail "parseInput: failed to parse"
