{-# LANGUAGE Strict #-}

module Common where
--import qualified Data.HashTable.IO as H

import Timer
import Data.List
import Data.Array.IO
import Data.IORef

--type HashTable k v = H.BasicHashTable k v --hashtable from hashtables

infixl 0 |>
(|>) :: a -> (a -> b) -> b
a |> f = f a

rr' = readIORef   --r for reference

rw' :: IORef a -> a -> IO a
rw' ref a = do {writeIORef ref a; pure a}

rm' = modifyIORef'
rn' = newIORef

{-# INLINE rr' #-}
{-# INLINE rw' #-}
{-# INLINE rm' #-}
{-# INLINE rn' #-}

println :: String -> IO ()
println = putStrLn

int :: Int -> Int
int a = a

--this operator is meant to be used as a record's field accessor
--actually, dont use it !
infixl 9 .>
{-# INLINE (.>) #-}
(.>) :: a -> (a -> b) -> b
(.>) record field = field record

infixl 9 # --flipped (.)
(#) :: (a -> b) -> (b -> c) -> (a -> c)
(#) f1 f2 = f2 . f1

infixl 9 ! --just another function application :)
(!) :: (a -> b) -> a -> b
(!) f = f

{-# INLINE (#) #-}
{-# INLINE (!) #-}


--experimental
for :: Int -> (Int -> Bool) -> (Int -> Int) -> b -> (Int -> b -> IO b ) -> IO b --functional for loop, processing some value at each step, returning the last one
for val pred step initial body
    | pred val = do
        res <- body val initial
        for (step val) pred step res body
    | otherwise = pure initial

--with IO condition
cfor :: Int -> IO (Int -> Bool) -> (Int -> Int) -> (Int -> IO b ) -> IO () --imperative for loop, just like in C !
cfor val pred step body = do
    pr <- pred
    if pr val then  do
        body val
        cfor (step val) pred step body
    else pure ()


cfor' :: (Num a) => a -> IO (a -> Bool) -> (a -> a) -> (a -> IO b ) -> IO () --imperative for loop, just like in C !
cfor' val pred step body = do
    pr <- pred
    if pr val then  do
        body val
        cfor' (step val) pred step body
    else pure ()


while :: (a -> IO Bool) -> a -> (a -> IO a) -> IO () --functional while loop, same as functional for loop
while predicate x func = do
      ok <- predicate x
      if ok
          then
              do
                    y <- func x
                    while predicate y func
          else
              return ()
------------------

timed :: (Integer -> String) -> IO a -> IO a
timed howToRender op = do
      t1 <- timeMs
      res <- op
      t2 <- timeMs

      println $ howToRender (t2 - t1)

      pure res


swap :: IOArray Int a -> Int -> Int -> IO ()
swap arr i j = do
  atI <- readArray arr i
  atJ <- readArray arr j
  writeArray arr i atJ
  writeArray arr j atI

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates = Data.List.foldl (\seen x -> if x `elem` seen
                then seen
                else seen ++ [x]) []

facSort :: IOArray Int Int -> IO ()
facSort arr = do
  len <- (+ 1) . snd <$> getBounds arr
  cfor 0 ( (<) <$> pure len ) (+ 1) $ \i ->
    cfor (i + 1) ( (<) <$> pure len ) (+ 1) $ \j -> do
      this <- readArray arr i
      next <- readArray arr j
      if this > next
        then swap arr i j
        else pure ()
