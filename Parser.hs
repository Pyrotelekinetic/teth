{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Parser where

import Data.Char
import Control.Applicative
import Control.Monad


data Parser a = MakeParser (String -> Maybe (a, String))
  deriving Functor

instance Monad Parser where
  return x = MakeParser $ \ s -> Just (x, s)
  p >>= f = MakeParser $ \ s ->
    case runParser p s of
      Just (x, s') -> runParser (f x) s'
      Nothing -> Nothing

instance Applicative Parser where
  pure = return
  mf <*> mx = do
    f <- mf
    x <- mx
    return $ f x

instance Alternative Parser where
  empty = MakeParser $ \ s -> Nothing
  p <|> q = MakeParser $ \ s ->
    case runParser p s of
      Nothing -> runParser q s
      Just x -> Just x

runParser :: Parser a -> String -> Maybe (a, String)
runParser (MakeParser f) s = f s

data Stream
  = Text String
  | RegEx Stream

-- always fails
failP :: Parser a
failP = MakeParser $ \s -> Nothing

-- always passes, consuming 1 character
passP :: Parser Char
passP = MakeParser $ \case
  c : cs -> Just (c, cs)
  _ -> Nothing

-- consumes a character that passes a predicate, p
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = MakeParser $ \case
  c : cs | p c -> Just (c, cs)
  _ -> Nothing

-- parses a specific character, c
charP :: Char -> Parser Char
charP c = satisfy (== c)

-- consumes all whitespace characters
spaceP :: Parser String
spaceP = many $ satisfy isSpace

wordP :: Parser String
wordP = do
  spaceP
  some (satisfy $ flip elem chars) where
    chars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['+', '-', '*', '/', '=']

wordP' :: (Char -> Bool) -> Parser String
wordP' p = do
  some $ satisfy p

-- parses any alphabetic string
lettersP :: Parser String
lettersP = some $ satisfy isAlpha

-- parses a given string
stringP :: String -> Parser String
stringP = \case
  [] -> return []
  x : xs -> do
    c <- charP x
    cs <- stringP xs
    return $ c : cs

-- parses an integer
numP :: Parser Integer
numP = do
  ns <- some $ satisfy isDigit
  return $ read ns

-- parses a parenthesized string
parensP :: Parser a -> Parser a
parensP p = do
  spaceP
  charP '('
  spaceP
  result <- p
  spaceP
  charP ')'
  return result

textP :: Parser Stream
textP = do
  t <- many passP
  return $ Text t

regExP :: Parser Stream
regExP = do
  r <- many passP
  return $ RegEx $ Text r

-- confirms complete parse
finishedP :: Maybe (a, String) -> Maybe a
finishedP = \case
  Just (a, "") -> Just a
  _ -> Nothing

-- deals with potentioal parse failure
unwrap :: Maybe a -> a
unwrap = \case
  Just x -> x

sedParser :: Parser Stream
sedParser = textP <|> regExP
