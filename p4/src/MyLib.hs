{-# LANGUAGE TemplateHaskell #-}
module MyLib (someFunc) where

import Control.Lens

data C = C1 Int
       | C2 String
       deriving (Eq,Show,Read)
data B = B { _mc :: Maybe [C] } deriving (Eq,Show,Read)
data A = A { _a :: (Int, Int)
           , _b :: [B]
           , _c :: C } deriving (Eq,Show,Read)

makePrisms ''C
makeLenses ''B
makeLenses ''A

exA1 = A { _c = C1 123
         , _a = (1,23)
         , _b = [B Nothing, B (Just [C1 5, C2 "str"])] }
-- exA1.b.c.<???>.<???>.val += "2"
exA2 = exA1 { _b = map chB (_b exA1) }
  where chB (B Nothing) = B Nothing
        chB (B (Just cs)) = B $ Just $ map chC cs
        chC (C1 x) = C1 x
        chC (C2 s) = C2 $ '2' : s

-- меняем второй компонент поля a
exA3 = exA1 & a . _2 .~ 10000
-- меняем глубоко в структуре (применяем функцию "добавить символ 2 в начало")
exA4 = exA1 & b . traverse . mc . _Just . traverse . _C2 %~ ('2':)

someFunc :: IO ()
someFunc = do
  print exA1
  print exA2
  -- поле а, второй компонент
  print $ exA1 ^. a . _2
  -- первый из списка значений поля b
  print $ exA1 ^? b . traverse
  -- список (плоский) всех значений по указанному пути
  print $ exA1 ^.. b . traverse . mc . _Just . traverse . _C2
  print exA3
  print exA4
