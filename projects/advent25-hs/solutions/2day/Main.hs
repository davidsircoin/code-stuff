{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR

main :: IO ()
main = do
    myfile <- TIO.readFile "../../inputfiles/input2.txt"
    let myranges = T.splitOn "," $ T.filter (/= '\n') myfile
        myrangesStr = map T.unpack myranges
        mylistofTuples = map (breakExc '-') myrangesStr
        refined = filter saveInvalid2 (concat [ [(read x :: Int) .. (read y :: Int)] | (x,y) <- mylistofTuples])
    print $ sum refined


breakExc :: Char -> String -> (String, String)
breakExc x [] = ("","")
breakExc x (y:ys) = if x == y then ("",ys)
                    else (y : fst next, snd next)
                    where next = breakExc x ys

y :: String
y = "12121212"

getDivisors :: Int -> [Int]
getDivisors n = [d | d <- [1 .. n-1], rem n d == 0]


splitListatIndex :: [a] -> Int -> ([a], [a])
splitListatIndex xs 0 = ([], xs)
splitListatIndex [] _ = ([],[])
splitListatIndex (x:xs) n = (x : fst next, snd next)
                           where next = (splitListatIndex xs (n-1))


divideList :: [a] -> Int -> [[a]]
divideList [] _ = []
divideList xs n = fst split  : divideList (snd split) n
                where split = splitListatIndex xs n 

checkElementEq :: Eq a => [a] -> Bool
checkElementEq [] = True
checkElementEq (x : xs) = all (== x) xs

saveInvalid2 :: Int -> Bool
saveInvalid2 id = if filter checkElementEq possibleSplits /= [] then True
                else False
                where possibleSplits = map (divideList (show id)) (getDivisors (length (show id)))
                      



checkEvenDigits :: Int -> Bool
checkEvenDigits x = if rem (length ( show x) ) 2 == 0 then True
                    else False

saveInvalid :: Int -> Bool
saveInvalid x = if fst split == snd split then True
                else False
                where split = splitListatIndex (show x) (quot (length (show x)) 2)




