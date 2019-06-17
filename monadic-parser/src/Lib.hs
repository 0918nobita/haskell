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

someFunc :: IO ()
someFunc = putStrLn "someFunc"
