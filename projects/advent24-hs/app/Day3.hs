module Day3 where

import Parsing as  P
import Data.Maybe



day3 :: IO ()
day3 = do
    myfile <- readFile "./inputs/input3.txt"
    let scanned = scan' myfile
    print $ sum [ x*y | (x,y) <- splitAtdont "do()" scanned ]


par3Dig = P.count 3 digit <|> P.count 2 digit <|> P.count 1 digit


-- Parser that gives us a string of the form (dig1, dig2)
parseMulOp :: Parser String
parseMulOp = do _ <- P.string "mul"
                _ <- P.char '('
                dig1 <- par3Dig
                _ <- P.char ','
                dig2 <- par3Dig
                _ <- P.char ')'
                return $ "(" ++ dig1 ++ "," ++ dig2 ++ ")"


justify = (\x -> Just x) -- just shorthand

-- scan' goes through a string, and picks out all occurrences given below. As a 
-- middle step, we turn everything into Maybes because that makes it easier to work
-- with, as well as just removing later, since we just need to apply mapfromJust on it.
-- Ending up with the sought after list of strings. Note that if there would have
-- been any valid "integers" of the form 0XXX, this implementation would not have worked.
scan' :: String -> [String]
scan' xs = map fromJust $ filter isJust (fst ( head parsedxs)) -- filter out all elements of type Nothing and remove the Just wrapper.
            where parsedxs =
                            parse (many (
                                justify <$>( parseMulOp 
                            <|> P.string "do()"
                            <|> P.string "don't()" )
                            <|> fmap (const Nothing) P.ignore)) xs


splitAtdont :: String -> [String] -> [(Int,Int)]
splitAtdont _ [] = []
splitAtdont cond (y : ys) | y == "do()" = splitAtdont y ys
                          | y == "don't()" = splitAtdont y ys
                          | cond == "don't()" = splitAtdont cond ys
                          | cond == "do()" = (read y) : splitAtdont cond ys



-- Idea: Create a parser that catches "mul(", one that catches a possibly 3 digit number followed by ',',
-- and then another 3 digit one followed by a '('. Put these together with the right connections, (when to 
-- use what essentially), and then put that all into a single parser, that we can use on myFile.
