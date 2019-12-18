getHead :: [a] -> Maybe (a, [a])
getHead (x:xs) = Just (x, xs)
getHead _      = Nothing

get3 :: [a] -> Maybe [a]
get3 xs0 = do
  (x1, xs1) <- getHead xs0
  (x2, xs2) <- getHead xs1
  (x3, _  ) <- getHead xs2
  return [x1, x2, x3]

main :: IO ()
main = do
  print $ get3 "abcd"  -- Just "abc"
  print $ get3 "a"     -- Nothing
