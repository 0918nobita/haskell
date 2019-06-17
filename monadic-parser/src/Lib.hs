module Lib
    ( someFunc
    ) where

newtype Parser a = Parser (String -> [(a, String)])

someFunc :: IO ()
someFunc = putStrLn "someFunc"
