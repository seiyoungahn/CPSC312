
module PlayfairCipher where

import Data.List
import Data.Char



{- DATA DEFINITIONS -}

type Digram = (Char, Char)

type Pos = (Int, Int)
type Matrix t = [(Pos, t)]
type CipherTable = Matrix Char

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

-- TODO - handle invalid input (i.e. numbers)

-- takes a piece of plaintext and a key; returns an encoded string 
encodePlayfair :: String -> String -> String
encodePlayfair plaintext key = fromDigrams (encryptDigrams (toDigrams (upcase plaintext)) (toCipherTable (upcase key)))

{--- CONVERSION ---}

-- converts a piece of plaintext into digrams
toDigrams :: String -> [Digram]
toDigrams plaintext = insertXs (head plaintext) (tail plaintext) 

-- converts a key string into a 5x5 cipher table
toCipherTable :: String -> CipherTable
toCipherTable key = zip positions (insertKey key) 

-- converts a list of digrams into a string
fromDigrams :: [Digram] -> String
fromDigrams digrams = foldr (\ (a,b) y -> a:b:y) [] digrams

{- CONVERSION HELPERS -}

-- converts a string to all uppercase characters
upcase :: String -> String
upcase str = [toUpper x | x <- str]

-- inserts a key into the alphabet as per Playfair rules; returns a 25 character long string
insertKey :: String -> String
insertKey key = uniqueChars ++ [x | x <- ['A'..'Z'], x `notElem` uniqueChars, x /= 'J'] where
  uniqueChars = nub key

-- converts characters into a list of digrams with Xs inserted as appropriate
insertXs :: Char -> [Char] -> [Digram]
insertXs x1 [] = [(x1, 'X')]
insertXs x1 [x2] = if x1 == x2 then [(x1, 'X'), (x2, 'X')] else [(x1, x2)]
insertXs x1 (x2:y)
  | x1 == x2 = (x1, 'X') : insertXs x1 y
  | otherwise = (x1, x2) : insertXs (head y) (tail y)


{--- ENCRYPTION ---}

-- encrypts a list of digrams using the given cipher table
encryptDigrams :: [Digram] -> CipherTable -> [Digram]
encryptDigrams digrams table = [ encrypt (a,b) table | (a,b) <- digrams]

-- encrypts a single digram
encrypt :: Digram -> CipherTable -> Digram
encrypt digram table
  | (sameRow digram table) = swap digram table shiftRight
  | (sameColumn digram table) = swap digram table shiftDown
  | otherwise = swapInRectangle digram table

{- ENCRYPTION HELPERS -}

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

-- returns an encrypted digram with characters in the same rectangle
swapInRectangle :: Digram -> CipherTable -> Digram
swapInRectangle digram table = (getElem (fst firstPos, snd secondPos) table, getElem (fst secondPos, snd firstPos) table) where
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

{- MATRIX HELPERS -}

-- gets the position of the element in the matrix
getPosition :: (Eq a) => a -> Matrix a -> Pos
getPosition elem matrix = head [pos | (pos, x) <- matrix, x == elem]

-- gets the element at the given position in the matrix
getElem :: (Eq a) => Pos -> Matrix a -> a
getElem pos matrix = head [elem | (x, elem) <- matrix, x == pos]



{- *** DECODER *** -}

-- takes a piece of ciphertext and a key; returns a decoded string
decodePlayfair :: String -> String -> String
decodePlayfair ciphertext key = fromDigrams (decryptDigrams (toDigrams(upcase ciphertext)) (toCipherTable(upcase key)))

-- encrypts a list of digrams using the given cipher table
decryptDigrams :: [Digram] -> CipherTable -> [Digram]
decryptDigrams digrams table = [ decrypt (a,b) table | (a,b) <- digrams]

-- encrypts a single digram
decrypt :: Digram -> CipherTable -> Digram
decrypt digram table
  | (sameRow digram table) = swap digram table shiftLeft
  | (sameColumn digram table) = swap digram table shiftUp
  | otherwise = swapInRectangle digram table

swap :: Digram -> CipherTable -> (Pos -> Pos) -> Digram
swap digram table fun = (getElem (fun firstPos) table, getElem (fun secondPos) table) where
  firstPos = getPosition (fst digram) table
  secondPos = getPosition (snd digram) table

-- TODO - this should be almost identical - just need to shift left and up instead of right and down


{- *** SOLVER *** -}

-- takes a piece of ciphertext and attempts to decode it
solvePlayfair :: String -> String
solvePlayfair ciphertext = ciphertext

generateRandomCipherTable = toCipherTable (permutateString "ABCDEFGHIKLMNOPQRSTUVWXYZ")


-- start with calling something like..
-- foo cipherText getRandomKey 
--  randomKey = 
--  randomKeyScore = evaluate (decodePlayfair (toCipherTable))
foo cipherText bestKey maxScore 300 = bestKey
foo cipherText bestKey maxScore count = 