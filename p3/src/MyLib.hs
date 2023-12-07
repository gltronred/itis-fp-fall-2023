module MyLib where

{-------------------------------------------------------------------------------

Санта Клаус засыпает и спит, пока его не разбудят либо все девять его оленей,
венувшихся с каникул, либо группа из трёх из десяти его эльфов-помощников. Если
Санту разбудили олени, он запрягает их в свои сани, развозит подарки и
распрягает оленей (которые уходят на каникулы). Если Санту разбудили эльфы, он
даёт каждому из эльфов задание по R&D подарков и отпускает эльфов делать дальше
R&D самостоятельно. Санта даёт приоритет оленям, если его одновременно будят и
эльфы, и олени.

-------------------------------------------------------------------------------}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Random

meetInStudy :: Int -> IO ()
meetInStudy id = putStr ("Elf " ++ show id ++ " meeting in the study\n")

deliverToys :: Int -> IO ()
deliverToys id = putStr ("Reindeer " ++ show id ++ " delivering toys\n")


elf1, reindeer1 :: Group -> Int -> IO ()
elf1      gp id = helper1 gp (meetInStudy id)
reindeer1 gp id = helper1 gp (deliverToys id)

helper1 :: Group -> IO () -> IO ()
helper1 group do_task = do
    (in_gate, out_gate) <- joinGroup group
    passGate in_gate
    do_task
    passGate out_gate


elf :: Group -> Int -> IO ThreadId
elf gp id = forkIO (forever (do elf1 gp id
                                randomDelay))

reindeer :: Group -> Int -> IO ThreadId
reindeer gp id = forkIO (forever (do reindeer1 gp id
                                     randomDelay))

randomDelay :: IO ()
-- Delay for a random time between 1 and 1,000,000 microseconds
randomDelay = do waitTime <- getStdRandom (randomR (1, 1000000))
                 threadDelay waitTime

{------------------------------------------------------------------------------}

data Gate = MkGate Int (TVar Int)

newGate :: Int -> STM Gate
newGate n = do
    tv <- newTVar 0
    return (MkGate n tv)

passGate :: Gate -> IO ()
passGate (MkGate n tv)
  = atomically (do n_left <- readTVar tv
                   check (n_left > 0)
                   writeTVar tv (n_left-1))

operateGate :: Gate -> IO ()
operateGate (MkGate n tv) = do
    atomically (writeTVar tv n)
    atomically (do n_left <- readTVar tv
                   check (n_left == 0))

{------------------------------------------------------------------------------}

data Group = MkGroup Int (TVar (Int, Gate, Gate))

newGroup :: Int -> IO Group
newGroup n = atomically (do g1 <- newGate n
                            g2 <- newGate n
                            tv <- newTVar (n, g1, g2)
                            return (MkGroup n tv))

joinGroup :: Group -> IO (Gate,Gate)
joinGroup (MkGroup n tv)
  = atomically (do (n_left, g1, g2) <- readTVar tv
                   check (n_left > 0)
                   writeTVar tv (n_left-1, g1, g2)
                   return (g1,g2))

awaitGroup :: Group -> STM (Gate, Gate)
awaitGroup (MkGroup n tv) = do
    (n_left, g1, g2) <- readTVar tv
    check (n_left == 0)
    new_g1 <- newGate n; new_g2 <- newGate n
    writeTVar tv (n,new_g1,new_g2)
    return (g1,g2)

{------------------------------------------------------------------------------}

santa :: Group -> Group -> IO ()
santa elf_gp rein_gp = do
    putStr "----------\n"
    (task, (in_gate, out_gate)) <- atomically (orElse
                     (chooseGroup rein_gp "deliver toys")
                     (chooseGroup elf_gp "meet in my study"))
    putStr ("Ho! Ho! Ho! let’s " ++ task ++ "\n")
    operateGate in_gate
              -- Now the helpers do their task
    operateGate out_gate
  where
    chooseGroup :: Group -> String -> STM (String, (Gate,Gate))
    chooseGroup gp task = do gates <- awaitGroup gp
                             return (task, gates)

someFunc :: IO ()
someFunc = do
    elf_group <- newGroup 3
    sequence_ [ elf elf_group n | n <- [1..10] ]

    rein_group <- newGroup 9
    sequence_ [ reindeer rein_group n | n <- [1..9] ]

    forever (santa elf_group rein_group)
