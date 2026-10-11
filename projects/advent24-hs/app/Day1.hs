module Day1 where

import qualified Data.Set as Set

main :: IO ()
main = do
   myfile <- readFile "./inputs/input1.txt"
   let mylines = lines myfile
   let numbers = unzip ( [(read x, read y) | [x,y] <- map words mylines] :: [(Int,Int)] )-- :: [(Int,Int)] is here because read needs to know what we are converting our strings to. 
   let list1 = quicksort' (fst numbers)
   let list2 = quicksort' (snd numbers)
   print (sum (map (countInst list2) (removeDuplicates list1)))

quicksort' :: [Int] -> [Int]
quicksort' [] = []
quicksort' (x : xs) = quicksort' smaller  ++ [x] ++ quicksort' bigger 
    where 
        smaller = [a | a <- xs, a <= x]
        bigger = [a | a <- xs, a > x]

-- sum the distance between two lists by their pairwise difference
sumpairs :: [Int] -> [Int] -> Int
sumpairs xs [] = 0
sumpairs [] ys = 0
sumpairs (x : xs) (y : ys) = abs(x - y) + sumpairs xs ys

-- counts the # of times a certain int appears in a list
countInst :: [Int] -> Int -> Int
countInst [] x  = 0
countInst (y : ys) x | x < y = 0
                     | x == y = x + countInst ys x
                     | x > y = 0 + countInst ys x

-- turns lists into sets and then lists again, removing any duplicates as a result
removeDuplicates :: (Ord a) => [a] -> [a] 
removeDuplicates = Set.toList . Set.fromList


