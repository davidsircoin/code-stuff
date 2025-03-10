data RoseTree a = N a [RoseTree a]
  deriving (Eq, Show)

leaves :: RoseTree a -> [a]
leaves (N x []) = [x]
leaves (N x xs) = concatMap leaves xs

flatten :: RoseTree a -> [a]
flatten (N x xs) = x : concatMap flatten xs

t1 :: RoseTree Int
t1 = N 0 [N 1 [], N 2 [], N 3 [N 4 [], N 5 []]]

t2 :: RoseTree Int
t2 = N 0 []

--   1   4
--  /   /
-- 0 - 3
--  \   \
--   2   5

main :: IO ()
main = do
  print (leaves t2)
  print (leaves t1)
  print (flatten t1)
