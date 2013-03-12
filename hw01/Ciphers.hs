module Ciphers
( encryptCaesar
, decryptCaesar
, encryptSubstitution
, decryptSubstitution
, encryptViginere
, decryptViginere
, encryptBlock
, decryptBlock
)
where

-- hodi se u testovani permutace u encryptSubstitution
import Data.List (sortBy, nub)

-- | Zjisti velikost daneho rozsahu (pocet pismen vcetne zadanych)
-- ekvivalentni k length ['\0'.. maxBound]
rangeSize :: Char -> Char -> Int
rangeSize a b = 1 + diff a b
    where
        diff :: Char -> Char -> Int 
        diff x y = abs $ fromEnum x - fromEnum y

-- | Velikost rozsahu hodnot, ve kterem pracuji cifry
range :: Int
range = rangeSize '\0' maxBound


-- | Zasifruje vstupni string dle dodane permutace.
--
--
-- Piklad:
--
-- >>> encryptSubstitution [('a', 'b'), ('b', 'c'), ('c', 'a')] "abcde"
-- "bcade"
--
-- >>> encryptSubstitution [('a', 'b'), ('b', 'c'), ('c', 'd')] "abcde"
-- *** Exception: Nejedna se o permutaci.
--
-- Pokud je list predstavujici permutaci prazdny, pak je vysledkem samotny plain text.
encryptSubstitution :: [(Char, Char)] -- ^ Permutation
                    -> String         -- ^ Plain text
                    -> String         -- ^ Cipher text
encryptSubstitution [] str = str
encryptSubstitution p s = if isPermutation p
                            then map snd $ subst p (map makeTuple s)
                            else error "Nejedna se o permutaci"
    where
        isPermutation :: [(Char, Char)] -> Bool
        isPermutation l = nub l == l && perm ((tail . sortedPerm) l) ((head . sortedPerm) l) 
            where
                perm :: [(Char, Char)] -> (Char, Char) -> Bool
                perm [] (a,b) = a == b
                perm (x:rx) a = (fst x == snd a) && perm rx (fst a, snd x)

                sortedPerm :: Eq a => [(a,a)] -> [(a,a)]
                sortedPerm = sortBy (\a b -> if snd a == fst b then LT else GT)
       
        makeTuple :: a -> (a,a)
        makeTuple x = (x,x)
         
        subst :: [(Char, Char)] -> [(Char, Char)] -> [(Char, Char)]
        subst [] str = str 
        subst (x:rx) str = subst rx (map (xchng x) str)
            where
                -- | pokud se shoduje prvni znak v tuple, tak ho nahrad permutaci
                xchng :: (Char, Char) -> (Char, Char) -> (Char, Char)
                xchng a b = if fst a == fst b then a else b

-- | Desifruje vstupni string dle dodane permutace.
--
-- Piklad:
--
-- >>> decryptSubstitution [('a', 'b'), ('b', 'c'), ('c', 'a')] "bcade"
-- "abcde"
--
-- >>> decryptSubstitution [('a', 'b'), ('b', 'c'), ('c', 'd')] "bcade"
-- *** Exception: Nejedna se o permutaci.
--
-- Pokud je list predstavujici permutaci prazdny, pak je vysledkem samotny cipher text.
decryptSubstitution :: [(Char, Char)] -- ^ Permutation
                    -> String         -- ^ Cipher text
                    -> String         -- ^ Plain text
decryptSubstitution l = encryptSubstitution ((reverse . swapTuples) l)
    where
        swapTuple :: (a,b) -> (b,a)
        swapTuple (a,b) = (b,a)
        
        swapTuples :: [(Char, Char)] -> [(Char, Char)]
        swapTuples = map swapTuple


-- | Ke kazdemu znaku vstupniho Stringu pricte dany posun modulo (length ['\0'..maxBound]).
--
-- Priklad:
--
-- >>> encryptCaesar 5 "abc"
-- "fgh"
encryptCaesar :: Int    -- ^ Posun
              -> String -- ^ Plain text
              -> String -- ^ Cipher text
encryptCaesar p = map (shift p)
    where
        shift :: Int -> Char -> Char
        shift n c = toEnum $ (n + fromEnum c) `mod` range

-- | Od kazdeho znaku vstupniho Stringu odecte dany posun modulo (length ['\0'..maxBound]).
--
-- Priklad:
--
-- >>> decryptCaesar 5 "fgh"
-- "abc"
decryptCaesar :: Int    -- ^ Posun
              -> String -- ^ Plain text
              -> String -- ^ Cipher text
decryptCaesar p = map (shift p)
    where
        shift :: Int -> Char -> Char
        shift n z = toEnum $ (fromEnum z - n) `mod` range

-- | Ke kazdemu znaku vstupniho Stringu pricte prislusny znak klice modulo (length ['\0'..maxBound]).
--
-- Priklad:
--
-- >>> encryptViginere "012" "3456"
-- "cegf"
--
-- @\'3\' + \'0\' = \'c\'@ jelikoz hodnota @\'3\'@ je 51, hodnota @\'0\'@ je 48 a hodnota @\'c\'@ je 99
--
-- Pokud je klic prazdny, pak je vysledkem samotny plain text.
encryptViginere :: String -- ^ Klic
                -> String -- ^ Plain text
                -> String -- ^ Cipher text
encryptViginere "" s = s
encryptViginere key text = zipWith shift ((concat . repeat) key) text
    where
        shift :: Char -> Char -> Char
        shift a b = toEnum $ (fromEnum a + fromEnum b) `mod` range

-- | Ke kazdemu znaku vstupniho Stringu pricte prislusny znak klice modulo (length ['\0'..maxBound]).
--
-- Priklad:
--
-- >>> decryptViginere "012" "cegf"
-- "3456"
--
-- @\'c\' - \'0\' = \'3\'@ jelikoz hodnota @\'c\'@ je 99, hodnota @\'0\'@ je 48 a hodnota @\'3\'@ je 51
--
-- Pokud je klic prazdny, pak je vysledkem samotny cipher text.
decryptViginere :: String -- ^ Klic
                -> String -- ^ Plain text
                -> String -- ^ Cipher text
decryptViginere key = encryptViginere (invertedKey key)
	where
        -- | obtocime klic tak, ze z toho vznikne funkce (-)
        invertedKey :: String -> String
        invertedKey = map (toEnum . negativeShift . fromEnum)
			where
			    negativeShift x = (range - x) `mod` range

-- | Kazdy blok dane delky (pripadne kratsi, jedna-li se o posledni blok) zasifruje dle prislusne funkce.
--
-- Priklad:
--
-- >>> encryptBlock [encryptViginere "012", encryptViginere "012", encryptCaesar 5] 4 "34563456abcd3456"
-- "cegfcegffghicegf"
--
-- Pokud je list funkci prazdny, nebo pokud je zadana delka bloku mensi nebo rovna nule, pak je vysledkem samotny plain text.
encryptBlock :: [String -> String] -- ^ Funkce
             -> Int                -- ^ Velikost bloku
             -> String             -- ^ Plain text
             -> String             -- ^ Cipher text
encryptBlock fl blockSize text
	| null fl = text
	| blockSize <= 0 = text
	| otherwise = concat $ zipWith ($) ((concat . repeat) fl) (textBlocks blockSize text)
    where
        textBlocks :: Int -> String -> [String]
        textBlocks _ "" = []
        textBlocks 0 _= []
        textBlocks size t = take size t : textBlocks size (drop size t)

-- | Kazdy blok dane delky (pripadne kratsi, jedna-li se o posledni blok) desifruje dle prislusne funkce.
--
-- Priklad:
--
-- >>> decryptBlock [decryptViginere "012", decryptViginere "012", decryptCaesar 5] 4 "cegfcegffghicegf"
-- "34563456abcd3456"
--
-- Pokud je list funkci prazdny, nebo pokud je zadana delka bloku mensi nebo rovna nule, pak je vysledkem samotny cipher text.
decryptBlock :: [String -> String] -- ^ Funkce
             -> Int                -- ^ Velikost bloku
             -> String             -- ^ Cipher text
             -> String             -- ^ Plain text
decryptBlock = encryptBlock

