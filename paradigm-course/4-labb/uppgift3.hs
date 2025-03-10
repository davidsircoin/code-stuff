-- We want a function from a list of integers to another list of integers containing the ordered indices of the input list, of which the corresponding element is greater or equal to 24
--
f :: [Int] -> [Int]
f list = map fst (filter (\(n, x) -> x >= 24) (zip [0 ..] list))
