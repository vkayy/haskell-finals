module Alloc where

import Data.Maybe
import Data.List

import Types
import Examples
import Data.List (minimum, minimumBy)
import Data.Ord (comparing)

------------------------------------------------------
--
-- Part I
--
count :: Eq a => a -> [a] -> Int
count x
  = length . filter (== x)

degrees :: Eq a => Graph a -> [(a, Int)]
degrees (ns, es)
  = map (\n -> (n, count n st + count n en)) ns
  where (st, en) = unzip es

neighbours :: Eq a => a -> Graph a -> [a]
neighbours n (ns, es)
  = [if st == n then en else st | (st, en) <- es, n == st || n == en]

removeNode :: Eq a => a -> Graph a -> Graph a
removeNode n (ns, es)
  = (filter (/= n) ns, filter (\(st, en) -> n /= st && n /= en) es)

------------------------------------------------------
--
-- Part II
--
colourGraph :: (Ord a, Show a) => Int -> Graph a -> Colouring a
colourGraph _ ([], _)
  = []
colourGraph mx g@(ns, _)
  = (n, c) : cMap
  where
    (_, n) = minimum [(dg, n) | (n, dg) <- degrees g]
    g'     = removeNode n g
    cMap   = colourGraph mx g'
    nbs    = neighbours n g
    avcs   = [1..mx] \\ [c | (n, c) <- cMap, n `elem` nbs]
    c      = if null avcs then 0 else minimum avcs

------------------------------------------------------
--
-- Part III
--
buildIdMap :: Colouring Id -> IdMap
buildIdMap cg
  = [(id, if c == 0 then id else "R" ++ show c) | (id, c) <- cg]

buildArgAssignments :: [Id] -> IdMap -> [Statement]
buildArgAssignments ids idm
  = [Assign r (Var id) | (id, r@(ch : _)) <- idm', ch == 'R']
  where idm' = map (\id -> (id, lookUp id idm)) ids

renameExp :: Exp -> IdMap -> Exp
-- Pre: A precondition is that every variable referenced in 
-- the expression is in the idMap. 
renameExp (Var id) idm
  = Var (lookUp id idm)
renameExp (Apply op e e') idm
  = Apply op (renameExp e idm) (renameExp e' idm)
renameExp e _
  = e

renameBlock :: Block -> IdMap -> Block
-- Pre: A precondition is that every variable referenced in 
-- the block is in the idMap. 
renameBlock (Assign id e : sts) idm
  | Var id' == e' = renameBlock sts idm
  | otherwise     = Assign id' e' : renameBlock sts idm
  where
    id' = lookUp id idm
    e'  = renameExp e idm
renameBlock (If e b b' : sts) idm
  = If (renameExp e idm) (renameBlock b idm) (renameBlock b' idm)
  : renameBlock sts idm
renameBlock (While e b : sts) idm
  = While (renameExp e idm) (renameBlock b idm) : renameBlock sts idm
renameBlock _ _
  = []

renameFun :: Function -> IdMap -> Function
renameFun (f, as, b) idMap
  = (f, as, buildArgAssignments as idMap ++ renameBlock b idMap)

-----------------------------------------------------
--
-- Part IV
--
buildIG :: [[Id]] -> IG
buildIG lvs
  = (ns, filter (\(n, n') -> any (\lv -> n `elem` lv && n' `elem` lv) lvs) cbs)
  where
    ns  = nub $ concat lvs
    cbs = nub [min (n, n') (n', n) | n <- ns, n' <- ns, n /= n']

-- 1h 6m

-----------------------------------------------------
--
-- Part V
--
liveVars :: CFG -> [[Id]]
liveVars cfg
  = liveVars' (map (\((_, use), _) -> use) cfg)
  where
    liveVars' :: [[Id]] -> [[Id]]
    liveVars' lvs
      | lvs == lvs' = lvs'
      | otherwise   = liveVars' lvs'
      where lvs' = [lv ++ (nub (use ++ concatMap (lvs !!) succ)  \\ [def])
                    | (((def, use), succ), lv) <- zip cfg lvs]

buildCFG :: Function -> CFG
buildCFG (_, _, b)
  = buildCFG' 0 b
  where
    buildCFG' :: Int -> Block -> CFG
    buildCFG' n (Assign id e : sts)
      | id == "return" = ((id, varsOf e), []) : buildCFG' (n + 1) sts
      | otherwise      = ((id, varsOf e), [n + 1]) : buildCFG' (n + 1) sts
    buildCFG' n (If e b b' : sts)
      = (("_", varsOf e), [n + 1, n + length bcfg + 1]) : bcfg ++ bcfg' ++ sts'
      where
        bcfg  = buildCFG' (n + 1) b
        bcfg' = buildCFG' (n + length bcfg + 1) b'
        sts'  = buildCFG' (n + length bcfg + length bcfg' + 1) sts
    buildCFG' n (While e b : sts)
      = (("_", varsOf e), [n + 1, n + length bcfg + 1]) : bcfg' ++ sts'
      where
        bcfg        = buildCFG' (n + 1) b
        bcfg'       = init bcfg ++ [((d, u), [n])]
        sts'        = buildCFG' (n + length bcfg + 1) sts
        ((d, u), _) = last bcfg
    buildCFG' _ _
      = []
    
    varsOf :: Exp -> [Id]
    varsOf (Var id)
      = [id]
    varsOf (Apply _ e e')
      = nub (varsOf e ++ varsOf e')
    varsOf _
      = []