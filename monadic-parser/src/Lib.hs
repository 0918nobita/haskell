module Lib ( someFunc, Parser, parse, item ) where

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

someFunc :: IO ()
someFunc = putStrLn "someFunc"
