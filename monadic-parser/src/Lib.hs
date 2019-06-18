module Lib ( someFunc ) where

newtype Parser a = Parser (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (Parser p) = p

item :: Parser Char
item = Parser (\cs -> case cs of
                  ""      -> []
                  (c:cs') -> [(c, cs')])

instance Functor Parser where
  fmap f p = Parser $ \src -> map (\(a, str) -> (f a, str)) $ (parse p) src

instance Applicative Parser where
  pure ast = Parser $ \src -> [(ast, "")]
  precede <*> succeed = Parser $ \src ->
      let fs = map fst (parse precede "") in
          concatMap (\(ast, str) -> map (\f -> (f ast, str)) fs) (parse succeed src)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
