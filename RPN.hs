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

-- <ファンクター則>
-- 以下のファンクター則 1, 2 を満たす → 適用に関する基本的な振る舞いが関数や他のファンクターと一致することが保証される

-- 1. fmap id = id
--     id ( a -> a ) でファンクター値を写した場合、ファンクター値が変化してはいけない
--     > fmap id (Just 3)
--     Just 3

-- instance Functor Maybe where
--     fmap f (Just x) = Just (f x)
--     fmap f Nothing = Nothing

-- 2. fmap (f . g) = fmap f . fmap g 
--     すべての x に対して fmap (f . g) x = fmap f (fmap g x) が成り立つ

-- fmap (f . g) Nothing = Nothing
-- fmap f (fmap g Nothing) = fmap f Nothing = Nothing

-- fmap (f . g) (Just x) = Just ((f . g) x) = Just (f (g x)) 
-- fmap f (fmap g (Just x)) = fmap f (Just (g x)) = Just (f (g x))

data CMaybe a = CNothing | CJust Int a deriving (Show)

-- Functor のインスタンスなのにファンクター則を満たせていない例
instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter + 1) (f x)

-- > fmap id (CJust 0 "haha")
-- CJust 1 "haha"
-- > id (CJust 0 "haha")
-- CJust 0 "haha"

myAction1 :: IO String
myAction1 = do
    a <- getLine
    b <- getLine
    return $ a ++ b

-- IO 型クラスは Applicative インスタンスなのでアプリカティブ・スタイルを利用できる
-- 動作は myAction1 と同じ
myAction2 :: IO String
myAction2 = (++) <$> getLine <*> getLine

-- instance Applicative IO where
--     pure = return
--     a <*> b = do
--         f <- a
--         x <- b
--         return (f x)

-- instance Applicative ((->) r) where
--     pure x = (\_ -> x)
--     f <*> g = \x -> f x (g x)

-- pure は、引数を無視して常にその値を返す関数
-- (->) r インスタンスに特化した pure の型は pure :: a -> (r -> a)

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ show $ solveRPN $ args !! 0
    result1 <- myAction1
    result2 <- myAction2
    mapM_ putStrLn [result1, result2]
