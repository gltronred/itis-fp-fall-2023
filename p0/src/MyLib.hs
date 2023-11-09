module MyLib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

rev :: [a] -> [a]
rev [] = []
rev (x:xs) = xs -- [x]
