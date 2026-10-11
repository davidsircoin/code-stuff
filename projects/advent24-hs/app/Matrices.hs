module Matrices where

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

detRec :: (Integral a) => a -> Matrix a -> a
detRec d Empty = d
detRec d (Cons x row col rest) = detRec x $ divide (addMatrix (scalarMul x rest) $ negMatrix (mulColRow col row)) d
    where
        divide m d = mapMatrix (`div` d) m


