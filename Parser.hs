module Parser where

import System.IO

-- foo lst = [(quadgram,score) | (quadgram:" ":score:"\n") <- lst] 

consi x y = y : x 

-- first call -> foo file [("", "")] True
foo :: [Char] -> [([Char], [Char])] -> Bool -> [([Char], [Char])]
foo [] acc quadgram = acc
foo lst acc quadgram =
    if (h == ' ') then (foo t acc (not quadgram))
    else if (h == '\n') then (foo t (("", "") : acc) (not quadgram))
    else if (quadgram) then (foo t ((quad ++ [h] , score) : (tail acc)) quadgram)
    else (foo t ((quad , score ++ [h]) : (tail acc)) quadgram)
    where
    h = head lst
    t = tail lst
    (quad, score) = head acc


run n = 
    do
        file <- readFile "quadgrams.txt"
        -- return file
        return (take n (foo file [("", "")] True))