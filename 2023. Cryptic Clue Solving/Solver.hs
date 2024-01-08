{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Solver where

import Data.List
import Data.Char

import Types
import WordData
import Clues
import Examples

------------------------------------------------------
-- Part I

punctuation :: String
punctuation 
  = "';.,-!?"

cleanUp :: String -> String
cleanUp
  = map toLower . filter (`notElem` punctuation)

split2 :: [a] -> [([a], [a])]
split2 xs
  = [splitAt i xs | i <- [1..length xs - 1]]

split3 :: [a] -> [([a], [a], [a])]
split3 xs
  = concat [(xs1, [], xs2) : map (\(xs1', xs1'') -> (xs1', xs1'', xs2))
    (split2 xs1) | (xs1, xs2) <- split2 xs]

uninsert :: [a] -> [([a], [a])]
uninsert xs
  = [(xs2, xs1 ++ xs3) | (xs1, xs2, xs3) <- split3 xs, not (null xs2)]

split2M :: [a] -> [([a], [a])]
split2M xs
  = sxs ++ [(y, x) | (x, y) <- sxs] 
  where
    sxs = split2 xs

split3M :: [a] -> [([a], [a], [a])]
split3M xs
  = sxs ++ [(z, y, x) | (x, y, z) <- sxs]
  where
    sxs = split3 xs

------------------------------------------------------
-- Part II

matches :: String -> ParseTree -> Bool
matches s (Synonym s')
  = s `elem` synonyms s'
matches s (Anagram _ s')
  = sort s == sort s'
matches s (Reversal _ t)
  = matches (reverse s) t
matches s (Insertion _ t1 t2)
  = any (\(s1, s2) -> matches s1 t1 && matches s2 t2) (uninsert s)
matches s (Charade _ t1 t2)
  = any (\(s1, s2) -> matches s1 t1 && matches s2 t2) (split2 s)
matches s (HiddenWord _ s')
  = s `isInfixOf` filter (/= ' ') ((init . drop 1) s')

evaluate :: Parse -> Int -> [String]
evaluate (d, _, t) l
  = filter ((== l) . length) (filter (`matches` t) (synonyms (unwords d)))

------------------------------------------------------
-- Part III

-- Given...
parseWordplay :: [String] -> [ParseTree]
parseWordplay ws
  = concat [parseSynonym ws,
            parseAnagram ws,
            parseReversal ws,
            parseInsertion ws,
            parseCharade ws,
            parseHiddenWord ws]
    
parseSynonym :: [String] -> [ParseTree]
parseSynonym s
  | null (synonyms s') = []
  | otherwise          = [Synonym s']
  where s' = unwords s

parseAnagram :: [String] -> [ParseTree]
parseAnagram s
  = [Anagram [unwords s1] (concat s2) | (s1, s2) <- split2M s,
                                        unwords s1 `elem` anagramIndicators]

parseReversal :: [String] -> [ParseTree]
parseReversal s
  = [Reversal [unwords s1] t | (s1, s2) <- split2M s,
                                unwords s1 `elem` reversalIndicators,
                                t <- parseWordplay s2]

parseInsertion :: [String] -> [ParseTree]
parseInsertion s
  = is ++ es
  where
    is = [Insertion [unwords ws] t1 t2 | (i1, ws, i2) <- split3 s,
                                          unwords ws `elem` insertionIndicators,
                                          t1 <- parseWordplay i1,
                                          t2 <- parseWordplay i2]
    es = [Insertion [unwords ws] t2 t1 | (e1, ws, e2) <- split3 s,
                                          unwords ws `elem` envelopeIndicators,
                                          t1 <- parseWordplay e1,
                                          t2 <- parseWordplay e2]

parseCharade :: [String] -> [ParseTree]
parseCharade s
  = bs ++ as
  where
    bs  = [Charade [unwords ws] t1 t2 | (b1, ws, b2) <- split3 s,
                              unwords ws `elem` beforeIndicators,
                              t1 <- parseWordplay b1, t2 <- parseWordplay b2]
    as  = [Charade [unwords ws] t2 t1 | (a1, ws, a2) <- split3 s,
                              unwords ws `elem` afterIndicators,
                              t1 <- parseWordplay a1, t2 <- parseWordplay a2]

parseHiddenWord :: [String] -> [ParseTree]
parseHiddenWord s
  = [HiddenWord [unwords s1] (unwords s2) | (s1, s2) <- split2M s,
                                        unwords s1 `elem` hiddenWordIndicators]

-- Given...
parseClue :: Clue -> [Parse]
parseClue clue@(s, n)
  = parseClueText (words (cleanUp s))

parseClueText :: [String] -> [Parse]
parseClueText s
  = [(d, l, wp') | (d, l, wp) <- split3M s, unwords l `elem` linkWords,
                  not (null (synonyms (unwords d))),
                  wp' <- parseWordplay wp]

solve :: Clue -> [Solution]
solve c@(_, n)
  = [(c, p, head (evaluate p n)) | p <- parseClue c, not (null (evaluate p n))]


------------------------------------------------------
-- Some additional test functions

-- Returns the solution(s) to the first k clues.
-- The nub removes duplicate solutions arising from the
-- charade parsing rule.
solveAll :: Int -> [[String]]
solveAll k
  = map (nub . map getSol . solve . (clues !!)) [0..k-1]

getSol :: Solution -> String
getSol (_, _, sol) = sol

showAll
  = mapM_ (showSolutions . solve . (clues !!)) [0..23]

