
{- Huffman Codes -}

import Data.Char
import Data.List

data Tree c = Leaf c Int | Branch (Tree c) (Tree c) Int
    deriving (Show, Eq, Ord, Read)

data Bit = Z | I
    deriving (Eq, Ord)

instance Show Bit where
    show Z = "0"
    show I = "1"
    showList [] = id
    showList (x:xs) = \out -> (show x) ++ showList xs out

{--- Decoding ---}
-- Notice that this should work for types more general than Char (our c).
-- Question:
decode :: Eq c => (Tree c, [Bit]) -> [c]
decode (t,xs) = decodeAux t t xs 

-- You may or may not wish to use a helper function as follows for
-- decode (this function will not be marked, and you can leave it
-- undefined if you don't use it):
decodeAux :: Eq c => Tree c -> Tree c -> [Bit] -> [c]
decodeAux (Leaf c _ ) t [] = [c]
decodeAux _ _ [] = []
decodeAux (Leaf c _ ) t xs = c : decodeAux t t xs
decodeAux (Branch lb rb _) t (x:xs) = if x == I then decodeAux rb t xs else decodeAux lb t xs 


{-- decompression --}

{- The input String has the following format:

   * An integer n coded as a sequence of digits.
   
   * This is followed by exact n characters, have a tree write with
     show, that can be read with read.

   * A sequence of 0's and 1's (characters) representing a sequence of bits.

   The output should be some text.

-}

decompress :: String -> String
decompress xs = decode ((read(take n xs')),conv((drop n xs')))
                where
                    n = read (numlth xs [])
                    xs' =  drop (lgd xs 0) xs

numlth :: [Char] -> [Char] -> [Char]
numlth [] _ = []
numlth (x:xs) ys = if isDigit x == True then numlth xs (ys++[x]) else ys   

lgd :: [Char] -> Int -> Int
lgd [] n = n
lgd (x:xs) n = if isDigit x == True then lgd xs (n+1) else n

conv :: [Char] -> [Bit]
conv [] = []
conv (x:xs) = if x == '1' then I : conv xs else Z : conv xs

{--- Decompression for a smarter compression algorithm: For a short
string or a random string, the Huffman code of the string is longer
than the string. In this case, we produce the original string with a '*'
at the front, indicating that no compression was performed. 

However, we need to simulate this using `charlength`, since we're
outputting a bitsequence as characters.  charlength is the bit-length
of a single character. We could change this to simulate a different
character encoding.  ---}

charlength :: Int
charlength = 8

--gives the length in "bits" of a string
memSize :: String -> Int
memSize s = 8 * (length s)

-- Smarter decompression, as discussed above. The input is either *
-- followed by a string, or as in the original decompression function:
decompress' :: String -> String
decompress' [] = []
decompress' (x:xs) = if x=='*' then xs else decompress xs

{--- Generate the frequency table ---}
--An element of the type Freq is a symbol together with its frequency.
type Freq c = (c,Int)

leaf :: Freq c -> Tree c
leaf (c,i) = Leaf c i

freq :: Tree c -> Int
freq (Leaf _ i) = i
freq (Branch _ _ i) = i

--Generates a frequency table. 
tabulate :: Eq c => [c] -> [Freq c]
tabulate xs = freq2 xs

freq1 ::Eq c => [c] -> [c] -> [(c,Int)]
freq1  _ [] = []
freq1 [] _ = [] 
freq1(x:xs) ys = (freqe ys x 0)  ++ freq1 xs ys

freq2 :: Eq c => [c] -> [(c,Int)]
freq2 xs = freq1 (rmdups(xs)) xs

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs

freqe :: Eq c =>[c] -> c -> Int -> [(c,Int)]
freqe [] y z = [(y,z)]
freqe (x:xs) y z = if x == y then freqe xs y (z+1) else freqe xs y z

{- End of part 1. Your tasks for part 2 begin here. -}

-- Produce a Huffman tree from a list of Huffman trees.
-- https://www.siggraph.org/education/materials/HyperGraph/video/mpeg/mpegfaq/huffman_tutorial.html
-- Question:
makeTree :: [Tree c] -> Tree c
-- Collects a list of trees into an optimal prefix tree.
makeTree (x:[]) = x
makeTree (x:y:xs) = makeTree (insert1 (merge x y) xs)

insert1 :: Tree c -> [Tree c] -> [Tree c]
insert1 t [] = [t]
insert1 t (x:xs)
    | freq t > freq x = x : insert1 t xs
    | otherwise = t : x : xs

-- You may wish to use a helper function such as this:
merge :: Tree c -> Tree c -> Tree c
merge u t = Branch u t (freq u + freq t)

-- Question:
-- Generate a tree from list of Freqs (using makeTree above):
generateTree :: [Freq c] -> Tree c
generateTree xs = makeTree (leaftr (order xs))

leaftr :: [Freq c] -> [Tree c]
leaftr [] = []
leaftr (x:xs) = (leaf x) : leaftr xs 

order :: [(c,Int)] -> [(c,Int)]
order xs = sortBy order' xs

order' :: Freq c -> Freq c -> Ordering
order' a b    | (snd a) > (snd b)  = GT 
              | (snd a) < (snd b)  = LT 
              | (snd a) == (snd b) = EQ


-- Encoding table.
-- A key is a key-value pair (an entry in a map/table).
type Key c = (c,[Bit])

-- The whole coding table
type CodingTable c = [Key c]

-- Question:
-- Given a tree, generates a coding table

makeTable :: Eq c => Tree c -> CodingTable c
makeTable t = makeTable' t []
 
makeTable' :: Eq c => Tree c -> [Bit] -> [(c,[Bit])]
makeTable' (Leaf t _) x = [(t,x)]
makeTable' (Branch lb rb _) x = (makeTable' lb (x++[Z])) ++ (makeTable' rb (x++[I]))

-- Question:
-- Takes a string of symbols to a bit string, based on a given coding table
encodeUsingTable :: Eq c => CodingTable c -> [c] -> [Bit]
encodeUsingTable _ [] = []
encodeUsingTable ys (x:xs) = (lookup1 ys x) ++ (encodeUsingTable ys xs)

lookup1 (x:xs) y = if (fst x) == y then snd x else lookup1 xs y

-- Question:
-- Encodes directly from the tree (more efficient).
encodeUsing :: Eq c => Tree c -> [c] -> [Bit]
encodeUsing _ [] = []
encodeUsing t (x:xs) = (encodeUsing' t x []) ++ (encodeUsing t xs)

encodeUsing' :: Eq c => Tree c -> c -> [Bit] ->[Bit]
encodeUsing' (Leaf c _) x ys = if c==x then ys else []
encodeUsing' (Branch lb rb _) x ys = (encodeUsing' lb x (ys++[Z])) ++ (encodeUsing' rb x (ys++[I]))

-- Question:
-- From a string of symbols, generate the coding tree and the encoding
encode :: Eq c => [c] -> (Tree c, [Bit])
encode xs = (f, g)
                 where
                        f = generateTree(order(tabulate xs))
                        g = encodeUsingTable(makeTable(generateTree(order(tabulate xs)))) xs

-- Encoding trees

-- Question:
-- Compressing a string. This should be the inverse of decompress.
-- That is, this should output a string of the form
--
-- n ++ t ++ c
--
-- Where,
--    * n is a read from an integer
--    * t is read from a tree, and contains exactly n characters.
--    * c is string of bits.
compress :: String -> String
compress xs = n ++ t ++ c   
                where
                t = show (fst(encode xs))
                c = show (snd(encode xs))
                n = show (length (t))

-- Question:
-- Smarter compression: if the encoded string is larger than the input string,
-- instead output the input string with a '*' in front.

compress' :: String -> String
compress' xs = if encstr xs > memSize (xs) then ('*':xs) else compress xs
                where 
                    t = memSize (show (fst(encode xs)))
                    u = memSize(show (length (show (fst(encode xs)))))
                    w = length(show (snd(encode xs)))
                    encstr xs = t + u + w
