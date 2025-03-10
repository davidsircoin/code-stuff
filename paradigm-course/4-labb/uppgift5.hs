skyffla :: [a] -> [a]
skyffla [] = []
skyffla list = evens ++ skyffla odds
  where
    evens = [x | (n, x) <- zip [0 ..] list, even n]
    odds = [x | (n, x) <- zip [0 ..] list, odd n]
