{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

main :: IO () 
main = do 
       myfile <- readFile "../inputfiles/input3.txt"
       let mylines = lines myfile
           banks = [ [read [j] :: Int | j <- bank] | bank <- mylines]
           batteries = [snd (foldl maxPair (1, -1) row) | row <- banks]

       print $ sum batteries 


ys :: [Int]
ys = [read [y] :: Int | y <- "234234234234278"]



maxPair :: (Int, Int) -> Int -> (Int, Int)
maxPair (bestDigit, bestPair) new = (newBestD, newBestP)
                                    where newBestD = max new bestDigit
                                          newBestP = max (bestDigit * 10 + new) bestPair

data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving Eq

mapToInt :: Digit -> Int
mapToInt Zero = 0
mapToInt One = 1
mapToInt Two = 2
mapToInt Three = 3
mapToInt Four = 4
mapToInt Five = 5
mapToInt Six = 6
mapToInt Seven = 7
mapToInt Eight = 8
mapToInt Nine = 9


data Dozen = Dozen Digit Digit Digit Digit Digit Digit Digit Digit Digit Digit Digit Digit
instance Ord Digit where
    (<) x y = if (mapToInt x) < (mapToInt y) then True else False
    (<=) x y = if (mapToInt x) <= (mapToInt y) then True else False

 
firstLargestD :: [Int] -> Dozen
firstLargestD (x : xs) = 


maxDozen ::
  

    

splitListatIndex :: [a] -> Int -> ([a], [a])
splitListatIndex xs 0 = ([], xs)
splitListatIndex [] _ = ([],[])
splitListatIndex (x:xs) n = (x : fst next, snd next)
                           where next = (splitListatIndex xs (n-1))


firstLargest :: [Int] -> (Int, Int)
firstLargest xs = foldl largest (head xsZipped) xsZipped
                  where xsZipped = zip xs [1..]

findMaxN :: Int -> [Int] -> String
findMaxN _ [] = ""
findMaxN 0 integer = ""
findMaxN n integer = show (fst largestN) ++ (findMaxN (n-1- (snd largestN)) newInteger)
                    where largestN = firstLargest (fst (splitListatIndex integer n))
                          newInteger = snd (splitListatIndex integer (snd largestN))

--largest :: Int -> Int -> Int
--largest x y = if x <= y then y
--               else x

largest :: (Int, Int) -> (Int, Int) -> (Int, Int)
largest x y  = if (fst x) < (fst y) then y
                else x

--firstLargest [Int] -> (Int,Int)
--firstLargest xs = foldl largest (head xsZip) xsZip
--                where xsZip = zip xs [1..]
--
--largestPair :: [Int] -> Int
--largestPair xs = 10 * (fst _) + (fst firstlargest 
--                where _ = firstLargest (init xs) 


