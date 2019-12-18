import Control.Monad.State (State, state, evalState)

getHead :: State [a] a
getHead = state (\ (x:xs) -> (x, xs))

get3 :: [a] -> [a]
get3 = evalState $ do
  x1 <- getHead
  x2 <- getHead
  x3 <- getHead
  return [x1, x2, x3]

main :: IO ()
main = do
  putStrLn $ get3 "abcd"  -- OK
  -- putStrLn $ get3 "a"  -- NG
