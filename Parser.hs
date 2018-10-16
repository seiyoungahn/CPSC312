module Parser where

import System.IO

-- read quadgrams textfile and convert it into list of tuples. both quadgrams and score are represented as string
parseQuadgrams :: [Char] -> [([Char], [Char])] -> Bool -> [([Char], [Char])]
parseQuadgrams [] acc quadgram = acc
parseQuadgrams lst acc quadgram =
    if (h == ' ') then (parseQuadgrams t acc (not quadgram))
    else if (h == '\n') then (parseQuadgrams t (("", "") : acc) (not quadgram))
    else if (quadgram) then (parseQuadgrams t ((quad ++ [h] , score) : (tail acc)) quadgram)
    else (parseQuadgrams t ((quad , score ++ [h]) : (tail acc)) quadgram)
        where
        h = head lst
        t = tail lst
        (quad, score) = head acc

-- convert parsed list of tuples so that the score becomes an integer
convertScore :: [([Char], [Char])] -> [([Char], Integer)]
convertScore lst = [ (quadgrams, read score :: Integer) | (quadgrams, score) <- lst ]

parse n = 
    do
        file <- readFile "quadgrams.txt"
        return (convertScore (take n (parseQuadgrams file [("", "")] True)))