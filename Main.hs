import qualified Data.Map as Map

-- レコード構文
data Person = Person { firstName :: String
    , lastName :: String
    , age :: Int } deriving (Eq, Show, Read)

-- 型定義
data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i * l + j * m + k * n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i * m) (j * m) (k * m)

-- 型定義
data LockerState = Taken | Free deriving (Show, Eq)

-- 型シノニム
type Code = String

-- 型シノニムの多相化
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken then Right code
        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100, (Taken, "ZD39I"))
    , (101, (Free, "JAH3I"))
    , (103, (Free, "IQSA9"))
    , (105, (Free, "QOTSA"))
    , (109, (Taken, "893JJ"))
    , (110, (Taken, "99292"))]

infixr 5 :-:
data List a = Empty | Cons { head :: a, tail :: List a } 
    | a :-: List a deriving (Show, Read, Eq, Ord)

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a = Node a (treeInsert x left) right
    | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True
    | x < a = treeElem x left
    | x > a = treeElem x right

-- 7.8 型クラス中級講座
-- Haskell に自動導出してもらうことで独自の型を標準型クラスのインスタンスにする方法を学んだ
-- ここからは独自の型クラス作り、そのインスタンスを手動で作る方法を学ぶ

-- 型クラスはインターフェースのようなもので、特定の振る舞い(等値判定, 順序比較, 列挙, ...)を定義する
-- 定義された通りに振る舞うことができる型は、その型クラスのインスタンスとなる
-- 型クラスの振る舞いは、型クラス関数を定義することで得られる
-- 型宣言だけして実装は後回しにしても構わない
-- 「ある型 T がある型クラス C のインスタンスである」→「型クラス C が定義する関数(メソッド)たちを型 T に対して使える」

-- Eq 型クラスは == と /= という関数(メソッド)を定義している

-- class Eq a where
--     (==) :: a -> a -> Bool
--     (/=) :: a -> a -> Bool
--     x == y = not (x /= y)
--     x /= y = not (X == y)

-- class Eq a where は Eq という新しい型クラスの定義が始まることを意味する
-- a は型変数で、将来 Eq 型クラスのインスタンスとなるであろう型を表す
-- 後から == 関数のシグネチャを確認すると (Eq a) => a -> a -> Bool になっている

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- instance キーワードを使ってインスタンスを作る
-- == と /= のどちらかを定義すればいい (最小完全定義)

instance Show TrafficLight where
    show Red = "Red light"
    show Green = "Green light"
    show Yellow = "Yellow light"

-- Eq は自動導出しても同じ効果が得られる
-- Show の自動導出を使うと値コンストラクタがそのまま文字列に変換されて出るだけ

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

-- id は、引数をひとつとって同じものを返すだけの標準ライブラリ関数

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno _ = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- instance Functor [] where
--     fmap = map

-- instance Functor Maybe where
--     fmap f (Just x) = Just (f x)
--     fmap f Nothing = Nothing

-- instance Functor (Either a) where
--     fmap f (Right x) = Right (f x)
--     fmap f (Left x) = Left x

sequenceA_ :: (Applicative f) => [f a] -> f [a]
sequenceA_ [] = pure []
sequenceA_ (x:xs) = (:) <$> x <*> sequenceA xs

-- data CoolBool = CoolBool { getCoolBool :: Bool }
newtype CoolBool = CoolBool { getCoolBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

main :: IO ()
main = do
    list <- sequence [getLine, getLine, getLine]
    print list
