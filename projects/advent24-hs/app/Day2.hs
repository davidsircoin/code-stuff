module Day2 where

main :: IO ()
main = do 
    myfile <- readFile "./inputs/input2.txt"
    let mywords = map words (lines myfile)
    let mydata = [[read x :: Int | x <- words] | words <- mywords]
    print (sum [toleranceChecker xs | xs <- mydata])

checkMono :: [Int] -> Int
checkMono [x] = 1
checkMono (x1 : x2 : xs) | (1 <= x2 - x1) && (x2 - x1 <= 3) = 1 + checkMono (x2 : xs) 
                         | otherwise = 0


checker :: [Int] -> Int
checker xs | checkMono xs == length xs = 1 
           | checkMono (reverse xs) == length xs = 1
           | otherwise = 0

toleranceChecker :: [Int] -> Int 
toleranceChecker xs | sum (map checker (minusLists xs)) >= 1 = 1
                    | otherwise = 0
            

-- generate all sublists of a list with one element less
minusLists :: [Int] -> [[Int]]
minusLists [] = []
minusLists (x : xs) = xs : map (x :) (minusLists xs)


-- [1,2,3,4] -> [ [2,3,4], [1,3,4], [1,2,4], [1,2,3]]

-- 1 2 3 2 1 unsafe, when checking that it's increasing, it fails at some point x_n n = 1-4, and
-- we should start trying the reverse. However, that cannot be the same function call but on the reversed
-- list, as this case will then just put it into an infinite loop.




--checkSafety :: [Int] -> Int
--checkSafety (x : xs) = if 
