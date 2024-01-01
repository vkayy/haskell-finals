module SOL where

import Data.List
import Data.Maybe

import Types
import TestData

printF :: Formula -> IO()
printF
  = putStrLn . showF
  where
    showF (Var v)
      = v
    showF (Not f)
      = '!' : showF f
    showF (And f f')
      = "(" ++ showF f ++ " & " ++ showF f' ++ ")"
    showF (Or f f')
      = "(" ++ showF f ++ " | " ++ showF f' ++ ")"

--------------------------------------------------------------------------
-- Part I

-- 1 mark
lookUp :: Eq a => a -> [(a, b)] -> b
-- Pre: The item being looked up has a unique binding in the list
lookUp 
  = (fromJust .) . lookup

-- 3 marks
vars :: Formula -> [Id]
vars (Var id)
  = [id]
vars (Not f)
  = sort . nub $ vars f
vars (And f f')
  = sort . nub $ vars f ++ vars f'
vars (Or f f')
  = sort . nub $ vars f ++ vars f'

-- 1 mark
idMap :: Formula -> IdMap
idMap
  = (`zip` [1..]) . vars

--------------------------------------------------------------------------
-- Part II

-- An encoding of the Or distribution rules.
-- Both arguments are assumed to be in CNF, so the
-- arguments of all And nodes will also be in CNF.
distribute :: CNF -> CNF -> CNF
distribute a (And b c)
  = And (distribute a b) (distribute a c)
distribute (And a b) c
  = And (distribute a c) (distribute b c)
distribute a b
  = Or a b

-- 4 marks
toNNF :: Formula -> NNF
toNNF (And f f')
  = And (toNNF f) (toNNF f')
toNNF (Or f f')
  = Or (toNNF f) (toNNF f')
toNNF (Not (Or f f'))
  = And (toNNF $ Not f) (toNNF $ Not f')
toNNF (Not (And f f'))
  = Or (toNNF $ Not f) (toNNF $ Not f')
toNNF (Not (Not f))
  = toNNF f
toNNF f
  = f


-- 3 marks
toCNF :: Formula -> CNF
toCNF
  = toCNF' . toNNF
  where
    toCNF' :: NNF -> CNF
    toCNF' (And f f')
      = And (toCNF f) (toCNF f')
    toCNF' (Or f f')
      = distribute (toCNF f) (toCNF f')
    toCNF' f
      = f

-- 4 marks
flatten :: CNF -> CNFRep
flatten f
  = flatten' (idMap f) f
  where
    flatten' :: IdMap -> CNF -> CNFRep
    flatten' idm (Or f f')
      = [concat $ flatten' idm f ++ flatten' idm f']
    flatten' idm (And f f')
      = flatten' idm f ++ flatten' idm f'
    flatten' idm (Not (Var id))
      = [[negate $ lookUp id idm]]
    flatten' idm (Var id)
      = [[lookUp id idm]]

--------------------------------------------------------------------------
-- Part III

-- 5 marks
propUnits :: CNFRep -> (CNFRep, [Int])
propUnits cnf
  | null ucs  = (cnf, [])
  | otherwise = (cnf', uc : ucs')
  where
    ucs          = [l | l:ls <- cnf, null ls]
    (uc: _)      = ucs
    cld          = filter (notElem uc) cnf
    ltd          = map (filter (/= -uc)) cld
    (cnf', ucs') = propUnits ltd

dp :: CNFRep -> [[Int]]
dp cnf
  | null cnf'     = [ucs']
  | any null cnf' = []
  | otherwise     = concatMap (map (ucs' ++)) [trem, frem]
  where
    (cnf', ucs')   = propUnits cnf
    ((uc : _) : _) = cnf'
    (trem, frem)   = (dp ([uc] : cnf'), dp ([-uc] : cnf'))

--------------------------------------------------------------------------
-- Part IV

-- Bonus 2 marks
allSat :: Formula -> [[(Id, Bool)]]
  = undefined