module Parser where

import System.IO 
import Data.HashMap as Map


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
convertScore :: [([Char], [Char])] -> [([Char], Float)]
convertScore lst = [ (quadgrams, read score :: Float) | (quadgrams, score) <- lst ]

-- get map with quadgram as the key and score as the value
getQuadgramMap :: IO (Map [Char] Float)
getQuadgramMap =
    do
        file <- readFile "quadgrams.txt"
        return (Map.fromList (convertScore (parseQuadgrams file [("", "")] True)))
