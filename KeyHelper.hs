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

-- returns the list with element removed
removeElement:: (Eq a) => a -> [a] -> [a] 
removeElement elem [] = []
removeElement elem (h:t) = if (elem == h) then (removeElement elem t) else (h : (removeElement elem t))

-- returns list with elements at two indexes swapped
swapTwo' a b lst = swapTwo'' a b lst lst 0
swapTwo'' a b [] lst index = []
swapTwo'' a b (h:t) lst index =
    (if (index == a) then lst!!b
    else if (index == b) then lst!!a
    else h) : (swapTwo'' a b t lst (index + 1))
