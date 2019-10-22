import qualified Data.Char as Char

main :: IO ()
main = do
    contents <- getContents
    putStr $ map Char.toUpper contents
