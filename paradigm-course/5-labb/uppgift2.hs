applyList :: [(a -> a)] -> a -> a
-- applyList [] x = x
applyList fs x = foldr (\f x' -> f x') x fs

main :: IO ()
main = do
  print (applyList [] 2)
