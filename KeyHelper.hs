module KeyHelper where

import System.Random

-- get random key with 25 letters

getRandomKey = getRandomKey' "ABCDEFGHIKLMNOPQRSTUVWXYZ"
getRandomKey' letters = do
    if (letters /= [])
        then do
            index <- randomRIO(0, ((length letters) - 1))
            let letter = letters!!index
            nextLetter <- getRandomKey' (removeElement letter letters)
            return (letter : nextLetter)
        else return []

-- swap two letters from a key chosen at random
swapTwo key =
    do
        index1 <- randomRIO(0, 24)
        index2 <- randomRIO(0, 24)
        return (swapTwo' index1 index2 key)

swapTwoRows key = 
    do
        row1 <- randomRIO(0, 4)
        row2 <- randomRIO(0, 4)
        let result = concat (swapTwo' row1 row2 (getRows key))

        if (result == key) then swapTwoRows key else return result

swapTwoCols key = 
    do
        col1 <- randomRIO(0, 4)
        col2 <- randomRIO(0, 4)
        let result = colConcat (swapTwo' col1 col2 (getCols key))

        if (result == key) then swapTwoCols key else return result

getRandomNum a b =
    do 
        num <- randomRIO(a, b) :: IO Integer
        return num

getRandomProbability =
    do 
        num <- randomIO :: IO Float
        return num 

-- TODO - implement methods for each of the types of swapping (simulated annealing)

-- returns the list with element removed
removeElement:: (Eq a) => a -> [a] -> [a] 
removeElement elem [] = []
removeElement elem (h:t) = if (elem == h) then (removeElement elem t) else (h : (removeElement elem t))

-- TODO - put in a check for identical swaps in here? 

-- returns list with elements at two indexes swapped
swapTwo' a b lst = swapTwo'' a b lst lst 0
swapTwo'' a b [] lst index = []
swapTwo'' a b (h:t) lst index =
    (if (index == a) then lst!!b
    else if (index == b) then lst!!a
    else h) : (swapTwo'' a b t lst (index + 1))

-- returns the 5 rows of a 25 character long key
getRows :: String -> [String]
getRows [] = []
getRows key = (take 5 key) : getRows (drop 5 key)

getCols :: String -> [String]
getCols [] = []
getCols key = [ getCol x key | x <- [0..4] ]

getCol :: Int -> String -> String
getCol col key = [ x | (i, x) <- (zip [0..] key), ((i `mod` 5) == col) ]

colConcat :: [String] -> String
colConcat [] = []
colConcat cols = [ head col | col <- cols, col /= [] ] ++ colConcat [ tail col | col <- cols, col /= []]





