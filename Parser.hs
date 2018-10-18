module Parser where

import System.IO 
import Prelude hiding (lookup)
import Data.List hiding (lookup) 
import Data.Map


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

-- get map with quadgram as the key and score as the value
getQuadgramMap =
    do
        file <- readFile "quadgrams.txt"
        return (fromList (convertScore (parseQuadgrams file [("", "")] True)))






-- for testing and looking up
-- parse = 
--     do
--         file <- readFile "quadgrams.txt"
--         putStrLn "parsing.."
--         let quadgramMap = fromList(convertScore (parseQuadgrams file [("", "")] True))
--         putStrLn ("size: " ++ (show (size quadgramMap)))
--         putStrLn "parsing done!"
--         getScore quadgramMap
--         return "program finished"
getScore quadgramMap =
   do
       putStrLn ("enter quadgram. size: " ++ (show (size quadgramMap)))
       quadgram <- getLine
       let score = lookup quadgram quadgramMap
       putStrLn (show score)
       getScore quadgramMap
       return "program finished"
