
module PlayfairCipher where

import Data.List as List
import Data.Char as Char
import Data.HashMap as Map
import Data.Maybe 

import KeyHelper
import Parser

{- DATA DEFINITIONS -}

type Digram = (Char, Char)

type Pos = (Int, Int)
type Matrix t = [(Pos, t)]
type CipherTable = Matrix Char
type QuadgramMap = Map [Char] Float

positions = [
  (1,1), (1,2), (1,3), (1,4), (1,5),
  (2,1), (2,2), (2,3), (2,4), (2,5),
  (3,1), (3,2), (3,3), (3,4), (3,5),
  (4,1), (4,2), (4,3), (4,4), (4,5),
  (5,1), (5,2), (5,3), (5,4), (5,5)]

{- *** ENCODER *** -}

-- takes a piece of plaintext and a key; returns an encoded string 
encodePlayfair :: String -> String -> String
encodePlayfair plaintext key = fromDigrams (encryptDigrams (toDigrams (format plaintext)) (toCipherTable (format key)))

-- encrypts a list of digrams using the given cipher table
encryptDigrams :: [Digram] -> CipherTable -> [Digram]
encryptDigrams digrams table = [ encrypt (a,b) table | (a,b) <- digrams]

-- encrypts a single digram
encrypt :: Digram -> CipherTable -> Digram
encrypt digram table
  | (sameRow digram table) = swap digram table shiftRight
  | (sameColumn digram table) = swap digram table shiftDown
  | otherwise = swapInRectangle digram table

{- *** DECODER *** -}

-- takes a piece of ciphertext and a key; returns a decoded string
decodePlayfair :: String -> String -> String
decodePlayfair ciphertext key = fromDigrams (decryptDigrams (toDigrams(format ciphertext)) (toCipherTable(format key)))

-- decrypts a list of digrams using the given cipher table
decryptDigrams :: [Digram] -> CipherTable -> [Digram]
decryptDigrams digrams table = [ decrypt (a,b) table | (a,b) <- digrams]

-- decrypts a single digram
decrypt :: Digram -> CipherTable -> Digram
decrypt digram table
  | (sameRow digram table) = swap digram table shiftLeft
  | (sameColumn digram table) = swap digram table shiftUp
  | otherwise = swapInRectangle digram table

{--- SHARED HELPERS ---}

{- CONVERSION HELPERS-}

-- converts a piece of plaintext into digrams
toDigrams :: String -> [Digram]
toDigrams plaintext = toDigrams' (head plaintext) (tail plaintext) 

-- converts a key string into a 5x5 cipher table
toCipherTable :: String -> CipherTable
toCipherTable key = zip positions (insertKey key) 

-- converts a list of digrams into a string
fromDigrams :: [Digram] -> String
fromDigrams digrams = List.foldr (\ (a,b) y -> a:b:y) [] digrams

-- converts a string to all uppercase characters, with spaces stripped
format :: String -> String
format str = [toUpper x | x <- str, x /= ' ', x `elem` ['a'..'z'] || x `elem` ['A'..'Z']]

-- inserts a key into the alphabet as per Playfair rules; returns a 25 character long string
insertKey :: String -> String
insertKey key = uniqueChars ++ [x | x <- ['A'..'Z'], x `notElem` uniqueChars, x /= 'J'] where
  uniqueChars = nub key

-- converts characters into a list of digrams with Xs inserted as appropriate, and Js changed to Is
toDigrams' :: Char -> [Char] -> [Digram]
toDigrams' x1 [] = [(x1, 'X')]
toDigrams' 'J' lst = toDigrams' 'I' lst
toDigrams' x1 [x2] = if x1 == x2 then [(x1, 'X'), (x2, 'X')] else [(x1, x2)]
toDigrams' x1 (x2:y)
  | x1 == 'J' = toDigrams' 'I' (x2:y)
  | x2 == 'J' = (x1, 'I') : toDigrams' (head y) (tail y)
  | x1 == x2 = (x1, 'X') : toDigrams' x1 y
  | otherwise = (x1, x2) : toDigrams' (head y) (tail y)

{- MATRIX HELPERS -}

-- gets the position of the element in the matrix
getPosition :: (Eq a) => a -> Matrix a -> Pos
getPosition elem matrix = head [pos | (pos, x) <- matrix, x == elem]

-- gets the element at the given position in the matrix
getElem :: (Eq a) => Pos -> Matrix a -> a
getElem pos matrix = head [elem | (x, elem) <- matrix, x == pos]

{- ENCRYPTION/DECRYPTION HELPERS -}

-- checks to see if the characters in the digram are in the same row
sameRow :: Digram -> CipherTable -> Bool
sameRow digram table = fst (getPosition first table) == fst (getPosition second table) where
  first = fst digram
  second = snd digram

-- checks to see if the characters in the digram are in the same column
sameColumn :: Digram -> CipherTable -> Bool
sameColumn digram table = snd (getPosition first table) == snd (getPosition second table) where
  first = fst digram
  second = snd digram

-- abstract helper method to return a new digram from the ciphertable based on the given function 
swap :: Digram -> CipherTable -> (Pos -> Pos) -> Digram
swap digram table fun = (getElem (fun firstPos) table, getElem (fun secondPos) table) where
  firstPos = getPosition (fst digram) table
  secondPos = getPosition (snd digram) table

-- shifts the position one column to the right
shiftRight :: Pos -> Pos
shiftRight (i, j) = if (j == 5) then (i, 1) else (i, j + 1)

-- shifts the position one row down
shiftDown :: Pos -> Pos
shiftDown (i, j) = if (i == 5) then (1, j) else (i + 1, j)

-- shifts the position one column to the left
shiftLeft :: Pos -> Pos
shiftLeft (i, j) = if (j == 1) then (i, 5) else (i, j - 1)

-- shifts the position one row up
shiftUp :: Pos -> Pos
shiftUp (i, j) = if (i == 1) then (5, j) else (i - 1, j)

-- returns an encrypted digram with characters in the same rectangle
swapInRectangle :: Digram -> CipherTable -> Digram
swapInRectangle digram table = (getElem (fst firstPos, snd secondPos) table, getElem (fst secondPos, snd firstPos) table) where
  firstPos = getPosition (fst digram) table
  secondPos = getPosition (snd digram) table


{- *** SOLVER *** -}

-- takes a piece of ciphertext and attempts to decode it using a hill-climbing approach
solvePlayfairHC :: String -> IO ()
solvePlayfairHC ciphertext = 
    do
        randomKey <- getRandomKey
        
        quadgramsMap <- getQuadgramMap

        putStrLn ("Parsing quadgrams file...")
        putStrLn ("Starting Key: " ++ randomKey)

        let decodeAttempt = decodePlayfair ciphertext randomKey
        let quadgrams = getQuadgrams decodeAttempt (length decodeAttempt)
        let score = scoreQuadgrams quadgrams quadgramsMap

        solveHC ciphertext score randomKey quadgramsMap 0

-- helper method for solvePlayfairHC
solveHC :: String -> Float -> String -> QuadgramMap -> Int -> IO ()
solveHC ciphertext maxScore key map acc = 
    do 
        newKey <- if (acc > 300) then getRandomKey else swapTwo key

        let decodeAttempt = decodePlayfair ciphertext newKey
        let quadgrams = getQuadgrams decodeAttempt (length decodeAttempt)
        let score = scoreQuadgrams quadgrams map

        if (score > maxScore)
            then do
                putStrLn ("Decode Attempt: " ++ decodeAttempt)
                putStrLn ("Score: " ++ (show score) ++ " - Key: " ++ newKey)
                solveHC ciphertext score newKey map 0
            else    
                solveHC ciphertext maxScore key map (acc + 1)

-- takes a piece of ciphertext and attempts to decode it using a simulated annealing approach
solvePlayfairSA :: String -> IO ()
solvePlayfairSA ciphertext =
    do
        let temperature = calculateTemp ciphertext

        randomKey <- getRandomKey
        quadgramsMap <- getQuadgramMap

        putStrLn ("Parsing quadgrams file...")
        putStrLn ("Starting Key: " ++ randomKey)

        let decodeAttempt = decodePlayfair ciphertext randomKey
        let quadgrams = getQuadgrams decodeAttempt (length decodeAttempt)
        let score = scoreQuadgrams quadgrams quadgramsMap

        putStrLn ("Starting Score: " ++ (show score))
        solveSA ciphertext score randomKey quadgramsMap temperature 0


-- helper method for solvePlayfairSA
solveSA :: String -> Float -> String -> QuadgramMap -> Float -> Integer -> IO ()
solveSA ciphertext maxScore key map temp acc =
    do
        randomNum <- getRandomNum 1 50

        let newKey = if (randomNum == 1) then swapTwoRows key
            else if (randomNum == 2) then swapTwoCols key
            else if (randomNum == 3) then flipTopBottom key
            else if (randomNum == 4) then flipLeftRight key
            else if (randomNum == 5) then flipAcross key
            else swapTwo key

        newKey <- if (acc > 50000) then getRandomKey else newKey     

        if (acc > 50000) then do putStrLn ("Got random key: " ++ newKey) else putChar('.')

        let decodeAttempt = decodePlayfair ciphertext newKey
        let quadgrams = getQuadgrams decodeAttempt (length decodeAttempt)
        let score = scoreQuadgrams quadgrams map

        let scoreDiff = maxScore - score 

        if (scoreDiff < 0) -- parent score is lower than the child
            then do
                putStrLn ("Decode Attempt: " ++ decodeAttempt)
                putStrLn ("Score: " ++ (show score) ++ " - Key: " ++ newKey)

                solveSA ciphertext score newKey map temp acc
            else do
                let probability = 1 / (2.718 ** (scoreDiff / temp))
                randomProb <- getRandomProbability

                if (probability > randomProb)
                    then do
                        putStrLn ("Decode Attempt: " ++ decodeAttempt)
                        putStrLn ("Score: " ++ (show score) ++ " - Key: " ++ newKey)

                        solveSA ciphertext score newKey map temp acc
                    else
                        solveSA ciphertext maxScore key map temp (acc + 1)


-- calculates the temperature based on the length of the text
calculateTemp :: String -> Float
calculateTemp text = fromIntegral((length text) - 84) * 0.087 + 10

-- takes a string and the length of the string and returns a list of quadgram permutations
getQuadgrams :: String -> Int -> [String]
getQuadgrams text length
    | (length >= 4) = (List.take 4 text) : getQuadgrams (tail text) (length - 1)
    | otherwise = []

-- takes a list of quadgrams and assigns it a total score
scoreQuadgrams :: [String] -> QuadgramMap -> Float
scoreQuadgrams quadgrams map = sum [logBase 10 ((fromMaybe 0.01 (scoreQuadgram x map)) / 4224127912.0) | x <- quadgrams]

-- takes a single quadgram and looks up the score for it
scoreQuadgram :: String -> QuadgramMap -> Maybe Float
scoreQuadgram quadgram map = Map.lookup quadgram map


