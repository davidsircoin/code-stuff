main :: IO ()
main = do
    myfile <- readFile "./inputfiles/input1.txt"
    let rotations = remakeList (lines myfile)
    print (foldl rotatePin (0,50) rotations)

 

remakeList :: [String] -> [Int]
remakeList [] = []
remakeList (x : xs) = if head x == 'R' then (read (tail x) :: Int) : remakeList xs
                      else - (read (tail x) :: Int) : remakeList xs


mylist :: [(Char, Int)]
mylist = [('L', 68), ('R', 30)]


-- n will always be between 0 and 99, n + rotation can be whatever, if we save how much more rotation is than 99, then we have a winding of number, plus the potential amount we get by adding rotation mod 100 to n, and we are done.

mylist2 :: [Int]
mylist2 = [- 68, - 30, 48, - 5, 60, - 55, - 1, - 99, 14, - 82]
 


rotatePin :: (Int, Int) -> Int -> (Int, Int)
rotatePin (winding, current) n | current == 0 = (nextWinding, n `mod` 100)
                               | current + (rem n 100) <= 0 || current + (rem n 100) >= 100 = (nextWinding + 1, (current + n) `mod` 100)
                               | otherwise = (nextWinding, (current + n) `mod` 100)
                               where nextWinding = winding + (quot (abs n) 100)


                                     



