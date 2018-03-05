import qualified Control.Monad as Monad

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

main :: IO ()
main = do
    line <- getLine
    Monad.when (line == "foo") $ do
        putStrLn "Foooooo!!!!"
        Monad.forM_ [1..10] $ \a -> do
            print a
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            main
