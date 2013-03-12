import Data.Char (ord)

rangeSize :: Char -> Char -> Int
rangeSize a b = 1 + (diff a b)
	where
		diff :: Char -> Char -> Int
		diff a b = abs $ fromEnum a - fromEnum b
