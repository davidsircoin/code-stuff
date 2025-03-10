mulString :: Integer -> String -> String
mulString n x
  | n <= 0 = ""
  | otherwise = x ++ mulString (n - 1) x

addChar :: Char -> String -> String
addChar c [] = ""
addChar c (x : xs)
  | null xs = [x]
  | otherwise = x : c : addChar c xs

main :: IO ()
main = do
  putStrLn (mulString 9 "HELLO")
  putStrLn (addChar 'j' "HELLO")
