import Data.Maybe
import Data.List
import Data.Ord (comparing)

type AttName = String

type AttValue = String

type Attribute = (AttName, [AttValue])

type Header = [Attribute]

type Row = [AttValue]

type DataSet = (Header, [Row])

data DecisionTree = Null |
                    Leaf AttValue | 
                    Node AttName [(AttValue, DecisionTree)]
                  deriving (Eq, Show)

type Partition = [(AttValue, DataSet)]

type AttSelector = DataSet -> Attribute -> Attribute

xlogx :: Double -> Double
xlogx p
  | p <= 1e-100 = 0.0
  | otherwise   = p * log2 p 
  where
    log2 x = log x / log 2

lookUp :: (Eq a, Show a, Show b) => a -> [(a, b)] -> b
lookUp x table
  = fromMaybe (error ("lookUp error - no binding for " ++ show x ++ 
                      " in table: " ++ show table))
              (lookup x table)

--------------------------------------------------------------------
-- PART I
--------------------------------------------------------------------

allSame :: Eq a => [a] -> Bool
allSame xs
  = length (nub xs) == 1

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove x
  = filter (\(k, _) -> k /= x)

lookUpAtt :: AttName -> Header -> Row -> AttValue
--Pre: The attribute name is present in the given header.
lookUpAtt an h r
  = lookUp an (zip (map fst h) r)

removeAtt :: AttName -> Header -> Row -> Row
removeAtt an h r
  = [v | (k, v) <- zip (map fst h) r, k /= an]

addToMapping :: Eq a => (a, b) -> [(a, [b])] -> [(a, [b])]
addToMapping kv@(k, v) map
  | Just vs <- lookup k map = (k, v : vs) : remove k map
  | otherwise               = (k, [v]) : map

buildFrequencyTable :: Attribute -> DataSet -> [(AttValue, Int)]
--Pre: Each row of the data set contains an instance of the attribute
buildFrequencyTable (an, avs) (h, rs)
  = map (\av -> (av, length $ filter (== av) rvs)) avs
  where rvs = concatMap (filter (`elem` avs)) rs

--------------------------------------------------------------------
-- PART II
--------------------------------------------------------------------

nodes :: DecisionTree -> Int
nodes (Leaf _)
  = 1
nodes (Node _ ats)
  = 1 + sum (map (nodes . snd) ats)
nodes _
  = 0

evalTree :: DecisionTree -> Header -> Row -> AttValue
evalTree Null _ _
  = ""
evalTree (Leaf av) _ _
  = av
evalTree (Node an ats) h r
  = evalTree (lookUp (lookUpAtt an h r) ats) h r

--------------------------------------------------------------------
-- PART III
--------------------------------------------------------------------

--
-- Given...
-- In this simple case, the attribute selected is the first input attribute 
-- in the header. Note that the classifier attribute may appear in any column,
-- so we must exclude it as a candidate.
--
nextAtt :: AttSelector
--Pre: The header contains at least one input attribute
nextAtt (header, _) (classifierName, _)
  = head (filter ((/= classifierName) . fst) header)

partitionData :: DataSet -> Attribute -> Partition
partitionData (h, rs) (an, avs)
  = map (\av ->
      (av,
        (h', map (removeAtt an h) (filter (\r -> av == lookUpAtt an h r) rs))
    )) avs
  where h' = remove an h

buildTree :: DataSet -> Attribute -> AttSelector -> DecisionTree
buildTree d@(h, rs) c@(cn, _) f
  | null rs     = Null
  | allSame cvs = Leaf cv
  | otherwise   = Node an $ map (\(av', d') -> (av', buildTree d' c f)) ps
  where
    a@(an, _)    = f d c
    ps           = partitionData d a
    cvs@(cv : _) = map (lookUpAtt cn h) rs

--------------------------------------------------------------------
-- PART IV
--------------------------------------------------------------------

entropy :: DataSet -> Attribute -> Double
entropy (_, []) _
  = 0.0
entropy d@(_, rs) a
  = negate . sum $ map (xlogx . (/ fromIntegral (length rs)) . fromIntegral) os
  where os = map snd $ buildFrequencyTable a d

gain :: DataSet -> Attribute -> Attribute -> Double
gain d@(_, rs) p@(pn, _) c
  = entropy d c - sum (zipWith (*) pps pes)
  where
    os  = map snd $ buildFrequencyTable p d
    pps = map ((/ fromIntegral (length rs)) . fromIntegral) os
    pes = map (\(_, d) -> entropy d c) (partitionData d p)

bestGainAtt :: AttSelector
bestGainAtt d@(h, _) c@(cn, _)
  = maximumBy (comparing (\p -> gain d p c)) (remove cn h)

--------------------------------------------------------------------

outlook :: Attribute
outlook 
  = ("outlook", ["sunny", "overcast", "rainy"])

temp :: Attribute 
temp 
  = ("temp", ["hot", "mild", "cool"])

humidity :: Attribute 
humidity 
  = ("humidity", ["high", "normal"])

wind :: Attribute 
wind 
  = ("wind", ["windy", "calm"])

result :: Attribute 
result
  = ("result", ["good", "bad"])

fishingData :: DataSet
fishingData
  = (header, table)

header :: Header
table  :: [Row]
header 
  =  [outlook,    temp,   humidity, wind,    result] 
table 
  = [["sunny",    "hot",  "high",   "calm",  "bad" ],
     ["sunny",    "hot",  "high",   "windy", "bad" ],
     ["overcast", "hot",  "high",   "calm",  "good"],
     ["rainy",    "mild", "high",   "calm",  "good"],
     ["rainy",    "cool", "normal", "calm",  "good"],
     ["rainy",    "cool", "normal", "windy", "bad" ],
     ["overcast", "cool", "normal", "windy", "good"],
     ["sunny",    "mild", "high",   "calm",  "bad" ],
     ["sunny",    "cool", "normal", "calm",  "good"],
     ["rainy",    "mild", "normal", "calm",  "good"],
     ["sunny",    "mild", "normal", "windy", "good"],
     ["overcast", "mild", "high",   "windy", "good"],
     ["overcast", "hot",  "normal", "calm",  "good"],
     ["rainy",    "mild", "high",   "windy", "bad" ]]

--
-- This is the same as the above table, but with the result in the second 
-- column...
--
fishingData' :: DataSet
fishingData'
  = (header', table')

header' :: Header
table'  :: [Row]
header' 
  =  [outlook,    result, temp,   humidity, wind] 
table' 
  = [["sunny",    "bad",  "hot",  "high",   "calm"],
     ["sunny",    "bad",  "hot",  "high",   "windy"],
     ["overcast", "good", "hot",  "high",   "calm"],
     ["rainy",    "good", "mild", "high",   "calm"],
     ["rainy",    "good", "cool", "normal", "calm"],
     ["rainy",    "bad",  "cool", "normal", "windy"],
     ["overcast", "good", "cool", "normal", "windy"],
     ["sunny",    "bad",  "mild", "high",   "calm"],
     ["sunny",    "good", "cool", "normal", "calm"],
     ["rainy",    "good", "mild", "normal", "calm"],
     ["sunny",    "good", "mild", "normal", "windy"],
     ["overcast", "good", "mild", "high",   "windy"],
     ["overcast", "good", "hot",  "normal", "calm"],
     ["rainy",    "bad",  "mild", "high",   "windy"]]

fig1 :: DecisionTree
fig1
  = Node "outlook" 
         [("sunny", Node "temp" 
                         [("hot", Leaf "bad"),
                          ("mild",Node "humidity" 
                                       [("high",   Leaf "bad"),
                                        ("normal", Leaf "good")]),
                          ("cool", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "temp" 
                         [("hot", Null),
                          ("mild", Node "humidity" 
                                        [("high",Node "wind" 
                                                      [("windy",  Leaf "bad"),
                                                       ("calm", Leaf "good")]),
                                         ("normal", Leaf "good")]),
                          ("cool", Node "humidity" 
                                        [("high", Null),
                                         ("normal", Node "wind" 
                                                         [("windy",  Leaf "bad"),
                                                          ("calm", Leaf "good")])])])]

fig2 :: DecisionTree
fig2
  = Node "outlook" 
         [("sunny", Node "humidity" 
                         [("high", Leaf "bad"),
                          ("normal", Leaf "good")]),
          ("overcast", Leaf "good"),
          ("rainy", Node "wind" 
                         [("windy", Leaf "bad"),
                          ("calm", Leaf "good")])]


outlookPartition :: Partition
outlookPartition
  = [("sunny",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","bad"],["hot","high","windy","bad"],
                   ["mild","high","calm","bad"],["cool","normal","calm","good"],
                   ["mild","normal","windy","good"]])),
     ("overcast",([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["hot","high","calm","good"],["cool","normal","windy","good"],
                   ["mild","high","windy","good"],["hot","normal","calm","good"]])),
     ("rainy",   ([("temp",["hot","mild","cool"]),("humidity",["high","normal"]),
                   ("wind",["windy","calm"]),("result",["good","bad"])],
                  [["mild","high","calm","good"],["cool","normal","calm","good"],
                   ["cool","normal","windy","bad"],["mild","normal","calm","good"],
                   ["mild","high","windy","bad"]]))]