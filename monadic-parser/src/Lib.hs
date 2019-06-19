module Lib
  ( someFunc
  , Parser
  , parse
  , item
  , parserA
  , parserB
  , parserC
  , satisfy
  , char
  , string
  , oneOf
  , many
  , some
  , parserD
  , parserE
  , parserF
  ) where

import Control.Applicative (Alternative, empty, (<|>), many, some)
import Control.Monad (MonadPlus, mzero)

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

item :: Parser Char
item = Parser (\cs -> case cs of
                ""      -> []
                (c:cs') -> [(c, cs')])

instance Functor Parser where
  fmap f p = Parser $ \src ->
    map (\(a, str) -> (f a, str)) $ (parse p) src

infixl 1 |>
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

instance Applicative Parser where
  pure ast = Parser $ \src -> [(ast, src)]
  precede <*> succeed = Parser $ \src ->
    parse precede src
      |> concatMap (\(f, str) ->
        parse succeed str
          |> map (\(ast, str') -> (f ast, str')))

instance Monad Parser where
  p >>= f = Parser $ \src ->
    parse p src
      |> concatMap (\(ast, str) -> parse (f ast) str)

parserA :: Parser String
parserA = item >>= \a -> ((\b -> [a, b]) <$> item)

parserB :: Parser String
parserB = (\a b -> [a, b]) <$> item <*> item

parserC :: Parser String
parserC = do
            a <- item
            b <- item
            return [a, b]

instance Alternative Parser where
  empty = Parser $ \_ -> []
  p <|> q = Parser $ \src -> parse p src ++ parse q src

instance MonadPlus Parser

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = item >>= (\ast -> if f ast then return ast else mzero)

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string ""     = return ""
string (c:cs) = char c >>= (\cutChar -> (cutChar :) <$> string cs)

oneOf :: String -> Parser Char
oneOf cs = satisfy (`elem` cs)

-- D ::= foo | bar | baz
parserD :: Parser String
parserD = string "foo" <|> string "bar" <|> string "baz"

-- E ::= ε | b
parserE :: Parser String
parserE = (return []) <|> ((: []) <$> char 'b')

-- F ::= a E c
parserF :: Parser String
parserF = (\a b c -> a:b ++ [c]) <$> char 'a' <*> parserE <*> char 'c'

someFunc :: IO ()
someFunc = putStrLn "someFunc"
