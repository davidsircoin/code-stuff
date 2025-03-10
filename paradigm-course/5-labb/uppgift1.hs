data RoseTree a = N a [RoseTree a]
  deriving (Eq, Show)

mapRoseTree :: (a -> b) -> RoseTree a -> RoseTree b
mapRoseTree f (N x rs) = N (f x) [mapRoseTree f r | r <- rs]

t1 :: RoseTree Int
t1 = N 0 [N 1 [], N 2 [], N 3 [N 4 [], N 5 []]]

t2 :: RoseTree Int
t2 = N 0 []

main :: IO ()
main = do
  print (mapRoseTree (+ 1) t1)
