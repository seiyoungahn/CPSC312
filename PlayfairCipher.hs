
module PlayfairCipher where

import Data.List as List
import Data.Char as Char
import Data.Map as Map
import Data.Maybe 

import KeyHelper
import Parser

{- DATA DEFINITIONS -}

type Digram = (Char, Char)

type Pos = (Int, Int)
type Matrix t = [(Pos, t)]
type CipherTable = Matrix Char
type QuadgramMap = Map [Char] Integer

positions = [
  (1,1), (1,2), (1,3), (1,4), (1,5),
  (2,1), (2,2), (2,3), (2,4), (2,5),
  (3,1), (3,2), (3,3), (3,4), (3,5),
  (4,1), (4,2), (4,3), (4,4), (4,5),
  (5,1), (5,2), (5,3), (5,4), (5,5)]

{- TESTING AREA -}

table = toCipherTable "HELLO"
{- 	1	2	3	4	5
1	H 	E 	L 	O 	A
2 	B 	C 	D 	F 	G
3	I 	K 	M 	N 	P
4 	Q 	R 	S 	T 	U
5 	V 	W 	X 	Y 	Z -}

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

-- takes a piece of ciphertext and attempts to decode it
solvePlayfair :: String -> IO ()
solvePlayfair ciphertext = 
    do
        randomKey <- getRandomKey
        -- let randomKey = "ABCDEFGHIKLMNOPQRSTUVWXYZ"
        
        quadgramsMap <- getQuadgramMap

        putStrLn ("Parsing quadgrams file...")
        putStrLn ("Starting Key: " ++ randomKey)

        let decodeAttempt = decodePlayfair ciphertext randomKey
        let quadgrams = getQuadgrams decodeAttempt
        let score = scoreQuadgrams quadgrams quadgramsMap

        solve ciphertext score randomKey quadgramsMap 0


solve :: String -> Integer -> String -> QuadgramMap -> Int -> IO ()
solve ciphertext maxScore key map acc = 
    do 
        newKey <- if (acc > 300) then getRandomKey else swapTwo key

        let decodeAttempt = decodePlayfair ciphertext newKey
        let quadgrams = getQuadgrams decodeAttempt
        let score = scoreQuadgrams quadgrams map

        if (score > maxScore)
            then do
                putStrLn ("Decode Attempt: " ++ decodeAttempt)
                putStrLn ("Score: " ++ (show score) ++ " - Key: " ++ newKey)
                solve ciphertext score newKey map 0
            else    
                solve ciphertext maxScore key map (acc + 1)



solvePlayfair2 ciphertext =
    do
        let temperature = calculateTemp ciphertext

        randomKey <- getRandomKey
        quadgramsMap <- getQuadgramMap

        putStrLn ("Parsing quadgrams file...")
        putStrLn ("Starting Key: " ++ randomKey)

        let decodeAttempt = decodePlayfair ciphertext randomKey
        let quadgrams = getQuadgrams decodeAttempt
        let score = scoreQuadgrams quadgrams quadgramsMap

        solve2 ciphertext score randomKey quadgramsMap temperature 0



solve2 :: String -> Integer -> String -> QuadgramMap -> Float -> Integer -> IO ()
solve2 ciphertext maxScore key map temp acc =
    do
        randomNum <- getRandomNum 1 50

        let newKey = if (randomNum == 1) then swapTwoRows key else if (randomNum == 2) then swapTwoCols key else swapTwo key

        newKey <- if (acc > 50000) then getRandomKey else swapTwo key     

        let decodeAttempt = decodePlayfair ciphertext newKey
        let quadgrams = getQuadgrams decodeAttempt
        let score = scoreQuadgrams quadgrams map

        let scoreDiff = maxScore - score 

        if (scoreDiff < 0)
            then do
                putStrLn ("Decode Attempt: " ++ decodeAttempt)
                putStrLn ("Score: " ++ (show score) ++ " - Key: " ++ newKey)

                solve2 ciphertext score newKey map temp acc
            else do
                let probability = 1 / (2.718 ^ (fromIntegral scoreDiff) / temp)
                randomProb <- getRandomProbability

                if (probability > randomProb)
                    then do
                        putStrLn ("Decode Attempt: " ++ decodeAttempt)
                        putStrLn ("Score: " ++ (show score) ++ " - Key: " ++ newKey)

                        solve2 ciphertext score newKey map temp acc
                    else
                        solve2 ciphertext maxScore key map temp (acc + 1)



calculateTemp :: String -> Float
calculateTemp text = fromIntegral((length text) - 84) * 0.087 + 10


-- takes a string and returns a list of quadgram permutations
getQuadgrams :: String -> [String]
getQuadgrams text 
    | (length text >= 4) = (List.take 4 text) : getQuadgrams (tail text)
    | otherwise = []

-- takes a list of quadgrams and assigns it a total score
scoreQuadgrams :: [String] -> QuadgramMap -> Integer
scoreQuadgrams quadgrams map = sum [fromMaybe 0 (scoreQuadgram x map) | x <- quadgrams]

-- takes a single quadgram and looks up the score for it
scoreQuadgram :: String -> QuadgramMap -> Maybe Integer
scoreQuadgram quadgram map = Map.lookup quadgram map


-- IT WAS THE BEST OF TIMES IT WAS THE WORST OF TIMES IT WAS THE AGE OF WISDOM IT WAS THE AGE OF FOOLISHNESS IT WAS THE EPOCH OF BELIEF IT WAS THE EPOCH OF INCREDULITY IT WAS THE SEASON OF LIGHT IT WAS THE SEASON OF DARKNESS IT WAS THE SPRING OF HOPE IT WAS THE WINTER OF DESPAIR WE HAD EVERYTHING BEFORE US WE HAD NOTHING BEFORE US WE WERE ALL GOING DIRECT TO HEAVEN WE WERE ALL GOING DIRECT THE OTHER WAY



-- generateRandomCipherTable = toCipherTable (permutateString "ABCDEFGHIKLMNOPQRSTUVWXYZ")


-- start with calling something like..
-- foo cipherText getRandomKey 
--  randomKey = 
--  randomKeyScore = evaluate (decodePlayfair (toCipherTable))
--foo cipherText bestKey maxScore 300 = bestKey
--foo cipherText bestKey maxScore count = 

-- generate a random key
-- take ciphertext and parse it into a list of quadgrams 
-- run that through the map generated by the parser to output a score
-- recursively pass that to the same function and keep the highest score (start with 0 I guess)
-- print to the console an output







