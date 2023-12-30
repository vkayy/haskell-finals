import Data.List (tails, maximumBy)
import Data.Ord (comparing)
data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix s s'
  = s == take (length s) s'

removePrefix :: String -> String -> String
removePrefix
--Pre: s is a prefix of s'
  = drop . length

suffixes :: [a] -> [[a]]
suffixes
  = init . tails

isSubstring :: String -> String -> Bool
isSubstring s s'
  = any (isPrefix s) (suffixes s')

findSubstrings :: String -> String -> [Int]
findSubstrings s s'
  = [i | (i, sub) <- zip [0..] (suffixes s'), isPrefix s sub]

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf l)
  = [l]
getIndices (Node ts)
  = concatMap (\(_, t) -> getIndices t) ts

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition xxs@(x:xs) yys@(y:ys)
  | x == y    = (x : res, xrem, yrem)
  | otherwise = ([], xxs, yys)
  where (res, xrem, yrem) = partition xs ys
partition xs ys
  = ([], xs, ys)

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' _ (Leaf l)
  = [l]
findSubstrings' s (Node [])
  = []
findSubstrings' s (Node ((a, t) : ts))
  | null srem = getIndices t
  | null arem = findSubstrings' srem t
  | otherwise = findSubstrings' s (Node ts)
  where (_, srem, arem) = partition s a 


------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert (s, n) (Node [])
  = Node [(s, Leaf n)]
insert (s, n) (Node ((a, t) : ts))
  | null comm = Node ((a, t) : ts')
  | null arem = Node ((a, insert (srem, n) t) : ts)
  | otherwise = Node ((comm, Node [(srem, Leaf n), (arem, t)]) : ts)
  where
    (comm, srem, arem) = partition s a
    Node ts' = insert (s, n) (Node ts)

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

repeatedSubstrings :: String -> String -> SuffixTree -> [String]
repeatedSubstrings s s' (Leaf _)
  = [s]
repeatedSubstrings s s' (Node ts)
  = concatMap (\(a, t) -> repeatedSubstrings s' (s' ++ a) t) ts

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring
  = maximumBy (comparing length) . repeatedSubstrings "" ""

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]

