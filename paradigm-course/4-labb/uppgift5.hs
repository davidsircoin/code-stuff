skyffla :: [a] -> [a]
skyffla [] = []
skyffla list = [x | (n, x) <- zip [0 ..] list, even n] ++ skyffla [x | (n, x) <- zip [0 ..] list, odd n]

main :: IO ()
main = print (skyffla [1 .. 100])
