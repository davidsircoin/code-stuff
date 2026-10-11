module Basicparse where

type Parser a = String -> [(a,String)]

return' :: a -> Parser a
return' v inp = [(v,inp)]

failure :: Parser a
failure _ = []

item :: Parser Char
item [] = []
item (x:xs) = [(x,xs)]

parse :: Parser a -> String -> [(a,String)]
parse p = p


p :: Parser (Char,Char)
p inp = (item inp, (item >>= \x -> item) inp
