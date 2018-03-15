import System.Environment

solveRPN :: (->) String Double
solveRPN = head . foldl foldingFunc [] . words
    where foldingFunc (x:y:ys) "+" = (y + x) : ys
          foldingFunc (x:y:ys) "-" = (y - x) : ys
          foldingFunc (x:y:ys) "*" = (y * x) : ys
          foldingFunc (x:y:ys) "/" = (y / x) : ys
          foldingFunc (x:y:ys) "^" = (y ** x) : ys
          foldingFunc (x:xs) "ln" = log x : xs
          foldingFunc xs numStr = read numStr : xs

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ show $ solveRPN $ args !! 0
