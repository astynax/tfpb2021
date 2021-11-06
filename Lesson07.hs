{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
-- | Parser combinators

module Lesson07 where

import Control.Applicative
import Data.Char (isSpace, isDigit)
import Text.Read (readMaybe)

newtype Parser a
  = Parser
    { runParser :: String -> [(a, String)] }

match :: Parser a -> String -> Maybe a
match p s =
  case runParser p s of
    [(x, "")] -> Just x
    _         -> Nothing

failed :: Parser a
failed = Parser $ \_ -> []

eof :: Parser ()
eof = Parser $ \s -> case s of
  "" -> [((), "")]
  _  -> []

anyChar :: Parser Char
anyChar = Parser $ \s -> case s of
  (x:xs) -> [(x, xs)]
  _      -> []

satisfy :: (a -> Bool) -> Parser a -> Parser a
satisfy pred p = Parser $
  filter (pred . fst) . runParser p

char :: Char -> Parser Char
char c = satisfy (== c) anyChar

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $
    fmap (\(x, s') -> (f x, s'))
    . runParser p

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \s -> [(x, s)]

  pf <*> px = Parser $
    concatMap (\(f, s') -> runParser (f <$> px) s')
    . runParser pf

instance Alternative Parser where
  empty = Parser $ \s -> []
  px <|> py = Parser $
    (<>) <$> runParser px <*> runParser py

string :: String -> Parser String
string ""     = pure ""
string (x:xs) = (:) <$> char x <*> string xs

between
  :: Parser before -> Parser a -> Parser after
  -> Parser a
between pb px pa = pb *> px <* pa

-- braced =
--   between
--     (char '(')
--     (many (satisfy (/= ')') anyChar))
--     (char ')')

sepBy :: Parser separator -> Parser a -> Parser [a]
sepBy ps px = (:) <$> px <*> many (ps *> px)

space :: Parser ()
space = () <$ many (satisfy isSpace anyChar)

arrayOf :: Parser a -> Parser [a]
arrayOf p =
  between (char '[') (space *> items <* space) (char ']')
  where
    items = sepBy (space *> char ',' <* space) p

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  px >>= f = Parser $
    concatMap (\(x, s') -> runParser (f x) s')
      . runParser px

int :: Parser Int
int =
  positive
  <|> negate <$> (char '-' *> positive)
  where
    positive = do
      ds <- digits
      case readMaybe ds of
        Just v  -> pure v
        Nothing -> failed
    digits = some (satisfy isDigit anyChar)

quoted :: Parser String
quoted = between (char '"') (many anyChar) (char '"')

data JSON
  = JNull
  | JBool Bool
  | JString String
  | JNumber Int
  | JArray [JSON]
  | JObject [(String, JSON)]
  deriving (Show)

json :: Parser JSON
json =
  JNull <$ string "null"
  <|> JBool True <$ string "true"
  <|> JBool False <$ string "false"
  <|> JString <$> quoted
  <|> JNumber <$> int
  <|> JArray <$> arrayOf json
  <|> JObject <$> object
  where
    object =
      between (char '{')
      (space *> items <* space)
      (char '}')
    items = sepBy (space *> char ',' <* space)
      $ do
        key <- quoted
        space
        char ':'
        space
        value <- json
        pure (key, value)
