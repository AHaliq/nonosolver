module Lib
    ( Tile (U, X, O),
      solve,
      solveOne,
      solveExpOne,
      solveLineOnly
    ) where

import Data.List
import qualified Data.Matrix as M
import qualified Data.Vector as V

data Tile = U | X | O
    deriving (Eq)
instance Show Tile where
    show U = "*"
    show O = "O"
    show X = "-"

type Range  = (Int,Int)   -- (start,end)
type HRT    = (Int,Range) -- (hint,(start,end))
type Partial= (HRT,[Range],Int) -- (pulled hint,uncovered O,next min s)
type TilePar= (Int,[(Range,[Range])],Tile) -- (bounds,[(partition range,[O range])],prev tile)

-- TYPE DECLARATIONS ----------------------------

test x = map 
-- determine if ranges of Hint length can fit in a partition Width
validWH :: Int -> [Int] -> Bool
validWH w [] = w >= 0
validWH w [x] = w >= x
validWH w (x:xs) = if w' > 0 then validWH w' xs else False
    where w' = w - x - 1

-- generate a list of hint range tuples given:
-- offset starting index
-- last index
-- list of hints; range length
baseRange :: Int -> Int -> [Int] -> [HRT]
baseRange _ _ [] = []
baseRange o l [x] = [(x, (o, l))]
baseRange o l (x:xs@(y:_)) =
    case baseRange (o + x + 1) l xs of
        ps@((_,(_,t)):_) -> (x, (o, t-y-1)):ps

-- given hints and a partition, RESULT: possible partition orderings
pullH :: [Int] -> (Range,[Range]) -> [[HRT]]
pullH hs ((s,e),rs) = pullHRT (baseRange s e hs) rs

-- constrains hint ranges under influence of O ranges
-- a partition's HRT, a partition's O ranges, RESULT: csp'ed partition HRT
pullHRT :: [HRT] -> [Range] -> [[HRT]]
pullHRT [] [] = [[]]
pullHRT [] x = []
pullHRT x [] = [x]
pullHRT (h:hs) rs = foldl (++) [] $ map (\x -> expandHRRP hs x) (partials h rs)

-- given a partial solution and remaining hint ranges, RESULT: generate partial for next and append them
expandHRRP :: [HRT] -> Partial -> [[HRT]]
expandHRRP [] (h,rs,_) = if null rs then [[h]] else []
expandHRRP ((l,(s,t)):hs) (h0@(l0,(s0,t0)),rs,ms) =
    let s' = max s ms in      -- respect previous hint's demand for min s
    if t-s'+1<l               -- respecting min s violates new range shorter than hint length
    then []
    else foldl (++) [] $ map (\x -> f <$> (expandHRRP hs x)) (partials (l,(s',t)) rs)
    where
        f [] = [h0]             -- respect next hint's demand for max t on h0
        f res@((_,(_,tn)):_) = if tn-l < s0-l0 then [] else (l0,(s0,min (tn-l-1) t0)):res

partials :: HRT -> [Range] -> [Partial]
partials h@(l,(s,_)) [] = [(h,[],s+l+1)]  -- no O, h remains the same
partials h@(l,(s,t)) rs@((o,p):_) =
    (if s+l-1 < o-1                         -- check if space exist before first
    then (((l,(s,o-2)),rs,s+l+1):)          -- append covering no O partial
    else id) (coverHR h rs)

-- generate partials of covering O ranges
-- hint range, O ranges, RESULT: resulting range * ranges not covered * next hint's min s
coverHR :: HRT -> [Range] -> [Partial]
coverHR x [] = []
coverHR h@(l,(s,t)) [(o,p)] = 
    let
        s' = max s (p-l+1)      -- new s' pulled by O's end
        t' = min t (o+l-1) in   -- new t' pulled by O's start
    if  p>t ||                -- violate O intersect right
        p-o+1>l ||            -- violate O longer than hint length
        t'-s'+1<l             -- violate new range shorter than hint length
    then [] else [((l,(s',t')),[],p+2)]
coverHR h@(l,(s,t)) rs@(r@(o,p):rt@((o2,p2):_)) =
    (case coverHR (l,(s,min t (o2-2))) [r] of
        []          -> id
        [(h',_,p')] -> ((h',rt,p'):) -- rectify ranges not covered list
    ) $ coverHR h (mergeO rs)    -- merge O for next partial
    where
        mergeO ((o1,_):(_,p2):rt) = (o1,p2):rt
        mergeO x = x

---- PARTITION GENERATOR --------------------------

-- determine if order is validWH for each partition group of hints
-- list of partition, partition of hints, RESULT: True hints can fit in partition
validOW :: [Range] -> [[Int]] -> Bool
validOW ps hss = foldl (&&) True $ map (\((a,b),hs) -> validWH (b-a+1) hs) $ zip ps hss

-- divpts generate insert hint group into partitions
-- null element, number of elements, elements, number of partitions WHERE m >= n, RESULT: order
ordhg :: a -> Int -> [a] -> Int -> [[a]]
ordhg e _ [] m = [replicate m e]
ordhg e n (x:xs) m = map (x:) (ordhg e (n-1) (xs) (m-1)) ++ (if m > n then map (e:) (ordhg e n (x:xs) (m-1)) else [])

-- generate all possible O pulls for an order
-- order of partitions (hint,part range,O ranges), RESULT: order of hint ranges
ordPart :: [([Int], (Range, [Range]))] -> [[HRT]]
ordPart x = map (foldl (++) []) $ sequence $ map (\(hs,p) -> pullH hs p) x 

-- ORDERING GENERATOR ---------------------------

-- generate division points from initial [1..partitions]
-- number of hints, initial group division points, RESULT: (_, grouped hints)
divpts :: Int -> [Int] -> (Int, [[Int]])
divpts 0 [] = (1, [])
divpts _ [] = (1, [[]])
divpts n xs@(x:xt) = case divpts n xt of
    (m,xss') -> (m + 1, (map (x:) xss') ++ if x < n - m then snd (divpts n (map (+1) xs)) else [])

-- group hints by division points
-- a list of hint, an element from divpts (length h)
divhin :: Int -> [a] -> [Int] -> [[a]]
divhin _ [] [] = []
divhin _ h [] = [h]
divhin i (h:hs) xs@(x:xt) =
    if i == x - 1 then [h]:(divhin (i+1) hs xt) else
    (\(a:as) -> (h:a):as) (divhin (i+1) hs xs)

-- DIVISION POINT GENERATOR ---------------------

-- transform a hint range into a range where O must exist
seqOcsp :: [HRT] -> [HRT]
seqOcsp [] = []
seqOcsp ((l,(s,t)):hs) = ((l,(t-l+1,s+l-1))):(seqOcsp hs)

-- given a list of orders; hint range in a sequence, merge them into one by minimum range satisfying all
seqOM :: [[HRT]] -> [HRT]
seqOM [] = []
seqOM [x] = x
seqOM (x1:x2:xs) = let x' = map (\((l,(s1,t1)),(_,(s2,t2))) -> (l,(max s1 s2, min t1 t2))) (zip x1 x2) in
    seqOM (x':xs)

-- given a merged list of orders, flag if its tile is confirmed O
seqOF :: Int -> [HRT] -> [Bool]
seqOF b hss = f 0 hss
    where
        f i [] = replicate (b-i+1) False
        f i hs@((l,(s,t)):_) = (i>=s && i<=t):(f (i+1) (g (i+1) hs))
        g i (h@(l,(s,t)):hs) = if t<s || i>s+l-1 then hs else (h:hs)

seqO :: Int -> [[HRT]] -> [Int]
seqO b x = conInd b $ seqOF b $ seqOM $ map seqOcsp x

-- O EVALUATION ---------------------------------

-- given bounds and an order; hint range of a sequence, flag if its tile is confirmed X
seqXF :: Int -> [HRT] -> [Bool]
seqXF b hss = f 0 hss
    where
        f i [] = replicate (b-i+1) True
        f i hs@((_,(s,t)):_) = (i<s):(f (i+1) (g (i+1) hs))
        g i (h@(_,(_,t)):hs) = if i>t then hs else (h:hs)

-- given a list of list of bools, merge them into one list by logical AND
seqXM :: [[Bool]] -> [Bool]
seqXM [] = []
seqXM [x1] = x1
seqXM (x1:x2:xs) = seqXM ((map (\(x,y) -> x && y) (zip x1 x2)):xs)

-- given bounds and a list of orders, RESULT: new confirmed X indices
seqX :: Int -> [[HRT]] -> [Int]
seqX b x = conInd b $ seqXM $ map (seqXF b) x

-- X EVALUATION ---------------------------------

-- get the index of TRUE in a list of bools
conInd :: Int -> [Bool] -> [Int]
conInd b fs = map (fst) $ filter (\(_,f) -> f) $ zip [0..b] $ fs

evalseq :: [Int] -> [Tile] -> Maybe ([Int],[Int])
evalseq h t = case gentile h t of
    (_,[])    -> Nothing
    (l,ords)  -> Just $ (\x -> (seqO l x, seqX l x)) $ ords

-- SEQUENCE EVALUATOR ---------------------------

gentile :: [Int] -> [Tile] -> (Int, [[HRT]])
gentile h x = (\(b,p) -> (b,genseq b h p)) (tileToSeq x)

-- bounds (highest tile), hints for a sequence, partitions (range + O ranges) of a sequence
genseq :: Int -> [Int] -> [(Range,[Range])] -> [[HRT]]
genseq _ [] par = if (foldl (+) 0 $ map (\(_,rs)-> length rs) par) == 0 then [[]] else []
-- if no hints; and no O ranges blank is an ordering
genseq b h par =
    foldl (++) [] $                                     -- collate all possible O pull
        map (\x -> ordPart $ zip x par) $               -- each order's possible O pull
        filter (validOW ps) $                           -- filter violate X / cant fit
        foldl (++) [] $                                 -- collate possible ordering
            map (\x -> ordhg [] (length x) x pc) $      -- divpts hint groups in partitions; order
            map (divhin 0 h) $                          -- hints in groups defined by division points
            foldl (++) [] $                             -- collate list of division points
                map (\x -> snd(divpts (length h) x)) $  -- divpts division points
                map (\x -> [1..x]) $                    -- initial group division point for hint list
                [0..((min pc (length h))-1)]            -- division point index
    where
        ps = map (fst) par
        pc = (length ps)

-- SEQUENCE GENERATOR ---------------------------

-- given a vector of tiles (0-?,1-O,2-X) generate sequence meta data
tileToSeq :: [Tile] -> (Int,[(Range,[Range])])
tileToSeq v = (\(a,b,_) -> (a,b)) $ f 0 v
    where
        f :: Int -> [Tile] -> TilePar
        f i [U] = (i,[((i,i),[])],U)
        f i [O] = (i,[((i,i),[(i,i)])],O)
        f i [X] = (i,[],X)
        f i (x:xs) = case f (i+1) xs of
            res@(_,_,X) -> if x == X
                            then res
                            else g (f i [x]) res
            res@(_,_,U) -> if x == U
                            then h i res
                            else if x == O
                                then k i res
                                else g' X res
            res@(_,_,O) -> if x == U
                            then h i res
                            else if x == O
                                then k' i res
                                else g' X res
        -- set next
        g' :: Tile -> TilePar -> TilePar
        g' n (i,xs,_) = (i,xs,n)
        -- append partition
        g :: TilePar -> TilePar -> TilePar
        g (_,x,n) (i,xs,_) = (i,x++xs,n)
        -- extend head range
        h :: Int -> TilePar -> TilePar
        h s (i,((_,t),rs):ps,_) = (i,((s,t),rs):ps,U)
        -- append O range
        k :: Int -> TilePar -> TilePar
        k s (i,((_,t),rs):ps,_) = (i,((s,t),(s,s):rs):ps,O)
        -- extend O range
        k' :: Int -> TilePar -> TilePar
        k' s (i,((_,t),((_,p):rs)):ps,_) = (i,((s,t),(s,p):rs):ps,O)

-- TILE GENERATOR -------------------------------

solveExpOne :: [[Int]] -> [[Int]] -> Maybe (M.Matrix Tile)
solveExpOne hr hc = evalLoop hr hc (M.matrix (length hr) (length hc) $ const U)

solveOne :: [[Int]] -> [[Int]] -> Maybe (M.Matrix Tile)
solveOne hr hc = case g m0 of
    []  -> Nothing
    x   -> Just (head x)
    where
        w = length hc
        m0 = M.matrix (length hr) w $ const U
        g m = maybe [] (starttrials) (evalLoop hr hc m)
        starttrials m = let xs = getUs w 0 (M.toList m) in if xs == [] then [m] else runtrials xs m
        runtrials [] m = []
        runtrials (x:xs) m = (g $ M.setElem O x m) ++ runtrials xs m

solve :: [[Int]] -> [[Int]] -> [M.Matrix Tile]
solve hr hc = nub $ g m0
    where
        w = length hc
        m0 = M.matrix (length hr) w $ const U
        g m = maybe [] (starttrials) (evalLoop hr hc m)
        starttrials m = let xs = getUs w 0 (M.toList m) in if xs == [] then [m] else runtrials xs m
        runtrials [] m = []
        runtrials (x:xs) m = (g $ M.setElem O x m) ++ runtrials xs m

getUs _ _ [] = []
getUs w i (O:xs) = getUs w (i+1) xs
getUs w i (X:xs) = getUs w (i+1) xs
getUs w i (U:xs) = (i `div` w + 1, i `mod` w + 1):getUs w (i+1) xs

-- evaluate all rows / columns sequentially
evalLoop hr hc m = evalR hr m >>= evalC hc >>= (\m' -> if m == m' then Just m' else evalLoop hr hc m')

evalR hr m = evalD coordR M.getRow hr m
evalC hc m = evalD coordC M.getCol hc m

evalD coordD getD hss m = f hss 0 m
    where
        f [] _ m = Just m
        f (hs:hst) d m =
            (\(os,xs) -> setmat X (coordD d xs) $ setmat O (coordD d os) m) -- apply confirmations
                <$> evalseq hs (V.toList $ getD (d+1) m)  -- evaluate sequence confirmations
                >>= (\m' -> f hst (d+1) m')             -- process for further sequences

coordR :: Int -> [Int] -> [(Int,Int)]
coordR i cs = map (\j -> (i+1,j+1)) cs
coordC :: Int -> [Int] -> [(Int,Int)]
coordC j cs = map (\i -> (i+1,j+1)) cs

setmat :: Tile -> [(Int,Int)] -> M.Matrix Tile -> M.Matrix Tile
setmat _ [] m = m
setmat t (c:cs) m = setmat t cs $ M.setElem t c m

-- CORE ALGO ------------------------------------

-- l is length, i is 0 based, return is 1 based
collateTrack :: V.Vector Bool -> Int -> [Int]
collateTrack v l = f 0 v
    where  f i v = if i == l then [] else (if v V.! i then ((i+1):) else id) $ f (i+1) v

-- c is 0 based, l is 1 based
setAndTrack :: Bool -> Int -> Tile -> [Int] -> M.Matrix Tile -> V.Vector Bool -> (M.Matrix Tile, V.Vector Bool)
setAndTrack _ _ _ [] m v = (m,v)
setAndTrack r l t (c:cs) m v = setAndTrack r l t cs m' v'
    where
        p = if r then (l,c+1) else (c+1,l)
        m' = M.setElem t p m
        v' = if m M.! p == U then v V.// [(c,True)] else v

-- i is 1 based
lineSolve :: Bool -> [Int] -> Int -> M.Matrix Tile -> V.Vector Bool -> Maybe (M.Matrix Tile, V.Vector Bool)
lineSolve row hs i m v = (\(os,xs) -> uncurry (s X xs) (s O os m v)) <$>
    evalseq hs (V.toList $ (if row then M.getRow else M.getCol) i m)
    where s = setAndTrack row i

-- i is 1 based
lineSolveDim :: Bool -> V.Vector [Int] -> M.Matrix Tile -> [Int] -> V.Vector Bool -> Maybe (M.Matrix Tile, V.Vector Bool)
lineSolveDim _ _ m [] v = Just (m,v)
lineSolveDim r hs m (i:is) v = lineSolve r (hs V.! (i-1)) i m v >>= (\(m',v') -> lineSolveDim r hs m' is v')

lineSolveStep :: V.Vector [Int] -> V.Vector [Int] -> Int -> Int -> M.Matrix Tile -> [Int] -> V.Vector Bool -> Maybe (M.Matrix Tile)
lineSolveStep hr hc w h m ri cv = lineSolveDim True hr m ri cv >>=
    (\(m',v') -> lineSolveDim False hc m' (collateTrack v' w) (V.replicate h False)) >>=
    (\(m',v') -> let ri' = (collateTrack v' h) in if null ri'
        then Just m'
        else lineSolveStep hr hc w h m' ri' (V.replicate w False))

-- STARTS all rows and columns
-- TODO start at targetted row column
lineSolver :: [[Int]] -> [[Int]] -> M.Matrix Tile -> Maybe (M.Matrix Tile)
lineSolver hr hc m = lineSolveStep (V.fromList hr) (V.fromList hc) w h m [1..h] (V.replicate w True)
    where
        h = length hr
        w = length hc

--solveLineString :: [[Int]] -> [[Int]] -> String
--solveLineString hr hc = maybe "no solution" M.prettyMatrix (lineSolver hr hc (M.matrix (length hr) (length hc) $ const U))

solveLineOnly :: [[Int]] -> [[Int]] -> Maybe (M.Matrix Tile)
solveLineOnly hr hc = lineSolver hr hc (M.matrix (length hr) (length hc) $ const U)
-- solveLineOnly        replaces solveExpOne
-- solveGuessFirst      replaces solveOne
-- solveGuessAll        replaces solve

-- LINE SOLVE ALGO ------------------------------

