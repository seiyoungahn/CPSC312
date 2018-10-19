module KeyHelper where

import System.Random

-- get random key with 25 letters
getRandomKey :: IO String
getRandomKey = getRandomKey' "ABCDEFGHIKLMNOPQRSTUVWXYZ"
getRandomKey' letters = do
    if (letters /= [])
        then do
            index <- randomRIO(0, ((length letters) - 1))
            let letter = letters!!index
            nextLetter <- getRandomKey' (removeElement letter letters)
            return (letter : nextLetter)
        else return []

-- swap two random letters from a 25-character long key 
swapTwo :: String -> IO String
swapTwo key =
    do
        index1 <- randomRIO(0, 24)
        index2 <- randomRIO(0, 24)
        return (swapTwo' index1 index2 key)

-- swap two random rows from a 25-character long key 
swapTwoRows :: String -> IO String
swapTwoRows key = 
    do
        row1 <- randomRIO(0, 4)
        row2 <- randomRIO(0, 4)
        let result = concat (swapTwo' row1 row2 (getRows key))

        if (result == key) then swapTwoRows key else return result

-- swap two random columns from a 25-character long key 
swapTwoCols :: String -> IO String
swapTwoCols key = 
    do
        col1 <- randomRIO(0, 4)
        col2 <- randomRIO(0, 4)
        let result = colConcat (swapTwo' col1 col2 (getCols key))

        if (result == key) then swapTwoCols key else return result

-- flips a 25-character long key along the horizontal axis
flipTopBottom :: String -> IO String
flipTopBottom key =
    do
        return (concat (swapTwo' 1 3 (swapTwo' 0 4 (getRows key))))

-- flips a 25-character long key along the vertical axis
flipLeftRight :: String -> IO String
flipLeftRight key =
    do
        return (colConcat (swapTwo' 1 3 (swapTwo' 0 4 (getCols key))))

-- flips a 25-character long key along the diagonal axis
flipAcross :: String -> IO String
flipAcross key =
    do
        return (flipAcross' key [(1,5),(2,10),(3,15),(4,20),(7,11),(8,16),(9,21),(13,17),(14,22),(19,23)])

-- helper method for flipAcross
flipAcross' :: String -> [(Int, Int)] -> String
flipAcross' key [] = key
flipAcross' key lst = flipAcross' (swapTwo' (fst (head lst)) (snd (head lst)) key) (tail lst)

-- gets a random Integer between a and b
getRandomNum :: Integer -> Integer -> IO Integer
getRandomNum a b =
    do 
        num <- randomRIO(a, b) :: IO Integer
        return num

-- gets a random Float between 0 and 1
getRandomProbability :: IO Float
getRandomProbability =
    do 
        num <- randomIO :: IO Float
        return num 

-- returns the list with element removed
removeElement:: (Eq a) => a -> [a] -> [a] 
removeElement elem [] = []
removeElement elem (h:t) = if (elem == h) then (removeElement elem t) else (h : (removeElement elem t))

-- returns list with elements at two indexes swapped
swapTwo' :: Int -> Int -> [a] -> [a]
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

-- returns the 5 columns of a 25 character long key
getCols :: String -> [String]
getCols [] = []
getCols key = [ getCol x key | x <- [0..4] ]

-- helper method for getCols
getCol :: Int -> String -> String
getCol col key = [ x | (i, x) <- (zip [0..] key), ((i `mod` 5) == col) ]

-- helper method for swapTwoCols
colConcat :: [String] -> String
colConcat [] = []
colConcat cols = [ head col | col <- cols, col /= [] ] ++ colConcat [ tail col | col <- cols, col /= []]
