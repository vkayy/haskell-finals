{-# LANGUAGE BlockArguments #-}

module Tries where

import Data.List hiding (insert)
import Data.Bits

import Types
import HashFunctions
import Examples

--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

countOnes :: Int -> Int
countOnes n
  | n > 15    = countOnes (shiftR n 4) + bitTable !! lsb
  | otherwise = bitTable !! n
  where lsb = n .&. (bit 4 - 1)

countOnesFrom :: Int -> Int -> Int
countOnesFrom i n
  = popCount $ n .&. (bit i - 1)

getIndex :: Int -> Int -> Int -> Int
getIndex n i s
  = shiftR n (i * s) .&. (bit s - 1)

-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace i xs y
  = let (bf, _ : af) = splitAt i xs in bf ++ y : af

-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt i y xs
  = let (bf, af) = splitAt i xs in bf ++ y : af

--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie _ fl (Leaf vs)
  = fl vs
sumTrie ft fl (Node _ sns)
  = sum $ map (sumTrie' ft fl) sns
  where
    sumTrie' :: (Int -> Int) -> ([Int] -> Int) -> SubNode -> Int
    sumTrie' ft fl (Term v)
      = ft v
    sumTrie' ft fl (SubTrie t)
      = sumTrie ft fl t

-- If you get the sumTrie function above working you can uncomment
-- these three functions; they may be useful in testing.

trieSize :: Trie -> Int
trieSize
  = sumTrie (const 1) length

binCount :: Trie -> Int
binCount
  = sumTrie (const 1) (const 1)

meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)

member :: Int -> Hash -> Trie -> Int -> Bool
member v _ (Leaf vs) _
  = v `elem` vs
member v h (Node bv sns) b
  | not taken        = False
  | Term v' <- sn    = v == v'
  | SubTrie t' <- sn = member v (shiftR h b) t' b
  where
    i     = getIndex h 0 b
    sn    = sns !! countOnesFrom i bv
    taken = testBit bv i

--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert f d b v t
  = insert' f v t 0
  where
    insert' :: HashFun -> Int -> Trie -> Int -> Trie
    insert' _ v (Leaf vs) _
      | v `elem` vs = Leaf vs
      | otherwise   = Leaf (v : vs)
    insert' f v t@(Node bv sns) l
      | l == d - 1       = Leaf [v]
      | not taken        = Node (setBit bv i) (insertAt n (Term v) sns)
      | SubTrie _  <- sn = Node bv (replace n sns sn')
      | Term v' <- sn    = if v == v' then t else Node bv (replace n sns sn'')
      where
        i          = getIndex (f v) l b
        taken      = testBit bv i
        n          = countOnesFrom i bv
        sn         = sns !! n
        SubTrie t' = sn
        Term v'    = sn
        sn'        = SubTrie (insert' f v t' (l + 1))
        sn''       = SubTrie (insert' f v (insert' f v' empty (l + 1)) (l + 1))

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie f d b
  = foldr (insert f d b) empty