-- We want a function from a list of integers to another list of integers containing the ordered indices of the input list where the element is greater or equal to 24
--
f :: [Int] -> [Int]
f list = map fst (filter (\(n, x) -> x >= 24) (zip [0 ..] list)

main :: IO ()
main = print (f [100, 22, 25, 28, 0])
