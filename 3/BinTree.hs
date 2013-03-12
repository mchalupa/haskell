module BinTree where

data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Show)

binMap :: (a -> b) -> BinTree a -> BinTree b
binMap _ Empty = Empty
binMap f (Node v l r) = Node (f v) (binMap f l) (binMap f r) 

depth :: Integral a => BinTree b -> a
depth Empty = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

