module Lib
    ( someFunc
    ) where

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

someFunc :: IO ()
someFunc = putStrLn "someFunc"
