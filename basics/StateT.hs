import Control.Monad.State (StateT(StateT), evalStateT)

getHead :: StateT [a] Maybe a
getHead = StateT gethead
  where
    gethead :: [a] -> Maybe (a, [a])
    gethead (x:xs) = Just (x, xs)
    gethead _      = Nothing

get3 :: [a] -> Maybe [a]
get3 = evalStateT $ do
  x1 <- getHead
  x2 <- getHead
  x3 <- getHead
  return [x1, x2, x3]

main :: IO ()
main = do
  print $ get3 "abcd"  -- Just "abc"
  print $ get3 "a"     -- Nothing
