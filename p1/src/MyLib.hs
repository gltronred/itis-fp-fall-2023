module MyLib where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.IO

-- Отделяем побочные эффекты "состояние" от "IO"
-- Можем комбинировать в рамках одной программы

getNextNumber :: IO Int
getNextNumber = do
  putStrLn "Next number, please"
  read <$> getLine

-- Вводить с клавиатуры числа
-- Суммировать в состояние (сумма, количество)
-- При вводе -1 вернём среднее
comp :: StateT (Int,Int) IO Double
comp = do
  x <- lift getNextNumber
  (s,n) <- get
  case x of
    -1 -> pure $ fromIntegral s / fromIntegral n
    _ -> do
      put (s + x, n + 1)
      comp

someFunc :: IO ()
someFunc = do
  avg <- evalStateT comp (0,0)
  putStrLn $ "Average: " ++ show avg
