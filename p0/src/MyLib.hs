module MyLib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = xs -- [x]

-- Напишите тесты к функции и функцию
--
-- Работает как zip, но если один список
-- длиннее, циклически переиспользует второй
--
-- > zipLong [1,2,3] "abc"
-- [(1,'a'),(2,'b'),(3,'c')]
--
-- > zipLong [1,2] "abcd"
-- [(1,'a'),(2,'b'),(1,'c'),(2,'d')]
--
-- > zipLong [] "abcd"
-- []
zipLong :: [a] -> [b] -> [(a,b)]
zipLong as bs = error "Implement me!"


data Tree a
  = Empty
  | Node
    { left :: Maybe (Tree a)
    , value :: a
    , right :: Maybe (Tree a)
    }
  deriving (Eq,Show,Read)

empty :: Tree a
empty = Empty

traversal :: Tree a -> [a]
traversal _ = []
