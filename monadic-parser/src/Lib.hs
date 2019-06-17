module Lib
    ( someFunc
    ) where

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

item :: Parser Char
item = Parser (\cs -> case cs of
                  ""      -> []
                  (c:cs') -> [(c, cs')])

instance Functor Parser where
  fmap f p = Parser $ \src -> map (\(a, str) -> (f a, str)) $ (parse p) src

someFunc :: IO ()
someFunc = putStrLn "someFunc"
