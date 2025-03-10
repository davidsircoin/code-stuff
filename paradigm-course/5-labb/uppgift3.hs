data Matrix a = Empty | Cons a [a] [a] (Matrix a)
  deriving (Eq, Show)

matrixToList :: Matrix a -> [[a]]
matrixToList Empty = []
matrixToList (Cons x row col rest) = (x : row) : zipWith (:) col (matrixToList rest ++ repeat [])

--                                               repeat [] makes sure that zipWith doesn't stop just
--                                               because rest is smaller than col

listToMatrix :: [[a]] -> Matrix a
listToMatrix [] = Empty
listToMatrix ([] : xs) = Empty
listToMatrix (row : rows) = Cons (head row) (tail row) [head y | y <- rows] (listToMatrix [drop 1 y | y <- rows])
--                                            could probably do less with an inbuilt functions that partitions
--                                            heads and tails of lists but cba

mapMatrix :: (a -> b) -> Matrix a -> Matrix b
mapMatrix f Empty = Empty
mapMatrix f (Cons x row col rest) = Cons (f x) (map f row) (map f col) (mapMatrix f rest) --map map map

negMatrix :: (Num a) => Matrix a -> Matrix a
negMatrix = mapMatrix (\x -> -x)

scalarMul :: (Num a) => a -> Matrix a -> Matrix a
scalarMul a = mapMatrix (a *)

addMatrix :: (Num a) => Matrix a -> Matrix a -> Matrix a
addMatrix m1 m2 = listToMatrix (zipWith (zipWith (+)) (matrixToList m1) (matrixToList m2)) -- zip some zip

mulColRow :: (Num a) => [a] -> [a] -> Matrix a
mulColRow m1 m2 = listToMatrix [map (x *) m2 | x <- m1]

ls1 :: [Integer]
ls1 = [1, 2, 3]

ls2 :: [Integer]
ls2 = [4, 5, 6]

-- 1                4   5   6
-- 2  X  [4 5 6] =  8  10  12
-- 3               12  15  18


detRec :: (Integral a) => a -> Matrix a -> a
detRec d Empty = d
detRec d (Cons x row col rest) = detRec x $ divide (addMatrix (scalarMul x rest) $ negMatrix (mulColRow col row)) d
    where
        divide m d = mapMatrix (`div` d) m

det :: (Integral a) => Matrix a -> a
det = detRec 1 


arr1 :: [[Integer]]
arr1 =
  [ [1, 2, 3],
    [4, 5, 6],
    [7, 8, 9],
    [10, 11, 12]
  ]

arr2 :: [[Integer]]
arr2 = [[4]]


m1 :: Matrix Integer
m1 = Cons 1 [2] [3] (Cons 4 [] [] Empty)

--   42 1 -2
--    3 1  2
--   -9 3  4
m2 :: Matrix Integer
m2 = Cons 42 [1,-2] [3,-9] m1

m3 :: Matrix Double
m3 =
  Cons
    2.0
    [1.0, 3.2, 4.1]
    [3.2, 1.4, 1.0]
    ( Cons
        9.9
        [5.323, 56.4123]
        [15.323, 7.4993]
        (Cons 0 [0] [0] (Cons 1 [] [] Empty))
    )

m4 :: Matrix Integer
m4 = Cons 11 [12, 13, 14] [1, 5] (Cons 2 [3, 4] [6] (Cons 7 [8] [] Empty))

-- 11 12 13 14
--  1  2  3  4
--  5  6  7  8

m5 :: Matrix Integer
m5 = Cons 1 [2, 3] [4, 7, 10] (Cons 5 [6] [8, 11] (Cons 9 [] [12] Empty))

-- 1  2  3
-- 4  5  6
-- 7  8  9
-- 10 11 12


m6 :: Matrix Integer
m6 = Cons 1 [2, 3] [4, 7, 10] (Cons 5 [6] [8, 11] (Cons 9 [] [12] Empty))
