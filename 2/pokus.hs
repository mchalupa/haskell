module ShortEncode
    ( cenc
    , cdec
    ) where

import Test.QuickCheck

chars :: String
chars = "0123456789abcdefghijklmnopqrstuvwxyz"

ll :: Integer
ll = fromIntegral (length chars)

cenc :: Integer -> String
cenc 0 = ""
cenc n = chars !! fromIntegral m : cenc d where
    (d,m) = (n-1) `divMod` ll

cdec :: String -> Integer
cdec = foldr (\ x y -> x + y * ll) 0 . map pos where
    pos = succ . fromIntegral . length . flip takeWhile chars . (/=)

prop_id =  (\s -> (cenc . cdec) s == s)
prop_id2 (NonNegative s)  = (cdec . cenc) s == s
prop_length (NonNegative n) = length (cenc n) <= length (show n)
