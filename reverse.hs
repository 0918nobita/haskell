import qualified Control.Monad as Monad

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

main :: IO ()
main = do
    line <- getLine
    Monad.when (line == "foo") $ do
        putStrLn "Foooooo!!!!"
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main
