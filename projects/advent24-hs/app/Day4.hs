module Day4 where

import Parsing as P
import Data.List
import Data.Maybe
import Control.Monad
day4p1 :: IO () 
day4p1 = do
        myFile <- readFile "./inputs/input4.txt"
        let myLists = lines myFile
        let myListsReversed = [reverse xs | xs <- myLists]
        let myListsReversedTrans = transpose myListsReversed
        let myListsTrans = transpose myLists
        let allsublists = nub $ myLists ++ myListsTrans ++ allDiagSublists myLists ++ allDiagSublists myListsTrans ++ allDiagSublists myListsReversed ++ allDiagSublists myListsReversedTrans
 --       let smaller = head myLists : [head (tail myLists)]
        let xmas = map (countSubstring "XMAS") allsublists
        let samx = map (countSubstring "SAMX") allsublists
        print $ sum (xmas ++ samx)

 
countSubstring :: String -> String -> Int
countSubstring sub str
  | null sub  = 0
  | otherwise = length . filter (isPrefixOf sub) . tails $ str
  where
    tails [] = []
    tails xs@(_:xs') = xs : tails xs'


allDiagSublists :: [[a]] -> [[a]]
allDiagSublists ([] : _) = []
allDiagSublists xs = diagonalSublist xs : allDiagSublists (map (drop 1) xs)


diagonalSublist :: [[a]] -> [a]
diagonalSublist [] = []
diagonalSublist ([] : _) = []
diagonalSublist (r : rows) = (head r) : diagonalSublist rest
    where rest = map (drop 1) rows


------ PART two code cause I am literal brainlet

-- sliding window memes. Idea is to do one of size 3x3 and count how many times it passes arrays like [[M, _, S], [_, A, _], [M, _, S]] (and all valid permutations of this)


day4p2 :: IO ()
day4p2 = do 
        myFile <- readFile "./inputs/input4.txt"
        let myLists = lines myFile
        let numbOfCols = length (head myLists)
        let numbOfRows = length myLists
        let windows = map (checkForXMAS . fromJust) $ filter isJust $ foldr (++) [] [[slidingWindow 3 (row,col) myLists | col <- [0 .. numbOfCols - 2]] |row <- [1..numbOfRows -2]]
        print (length (filter (== True) windows))
        

slidingRow :: Int -> Int -> [a] -> Maybe [a]
--slidingWindow length 0 xs = take length xs
slidingRow len pos xs = if (length (drop pos xs)) >= len 
                            then Just (take len (drop pos xs)) 
                        else Nothing

slidingWindow :: Int -> (Int,Int) -> [[a]] -> Maybe [[a]] -- a sliding square window of size nxn
slidingWindow size (prow, pcol) xs = do
                                    guard (prow > size `div` 2 - 1)
                                    mapM (\n -> slidingRow size pcol (xs !! n)) [prow - (size `div` 2) .. prow + (size `div` 2)]

checkForXMAS :: [[Char]] -> Bool
checkForXMAS [['M', _, 'M'], [_,'A',_], ['S', _, 'S']] = True
checkForXMAS [['M', _, 'S'], [_,'A',_], ['M', _, 'S']] = True
checkForXMAS [['S', _, 'M'], [_,'A',_], ['S', _, 'M']] = True
checkForXMAS [['S', _, 'S'], [_,'A',_], ['M', _, 'M']] = True
checkForXMAS _ = False


