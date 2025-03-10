applyList :: [(a -> a)] -> a -> a
applyList fs x = foldr (\f x' -> f x') x fs
