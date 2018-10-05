{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}

module StackedDag.Base where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Maybe(maybe)

type Edges a = M.Map a (S.Set a)

type Labels a b = M.Map a b

data Symbol b =
    SNode b -- o with label
  | SLeft -- '/'
  | SRight -- '\\'
  | SHold -- '|'
  | SLMove -- '_'
  | SRMove -- '_'
  | SCross -- 'x'
  | SSpace -- ' '
  deriving (Show, Read, Eq)

appendSymbol :: Symbol b -> Symbol b -> Symbol b
appendSymbol n@(SNode _) _  = n
appendSymbol _ n@(SNode _) = n
appendSymbol a SSpace = a
appendSymbol SSpace a = a
appendSymbol SLeft SRight = SCross
appendSymbol SRight SLeft = SCross
appendSymbol SCross SRight = SCross
appendSymbol SCross SLeft = SCross
appendSymbol SRight SCross = SCross
appendSymbol SLeft SCross = SCross
appendSymbol a _ = a

instance Monoid (Symbol b) where
  mempty = SSpace
#if MIN_VERSION_base(4,11,0)
instance Semigroup (Symbol b) where
  (<>) = appendSymbol
#else
  mappend = appendSymbol
#endif

type Nodes a = S.Set a

type Depth = Int
type Dest = Int
type Cur = Int
type Pos = Int

type DepthNode a = M.Map a Depth

type DepthGroup a = M.Map Depth [a]

type NodeDepth a = M.Map a Depth

type DepthGroup' a = M.Map Depth ([a],[a])

type DepthGroup'' a = M.Map Depth ([(a,Cur,Dest)],[(a,Cur,Dest)])

mkEdges :: Ord a => [(a,[a])] -> Edges a
mkEdges edges = M.fromList $ map (\(nid,nids) -> (nid, S.fromList nids)) g
  where
    g = map (\xs@((k,_):_)-> (k,concat $ map snd xs) ) $ L.groupBy (\(a,_) (b,_) -> a == b) $  L.sortBy (\(a,_) (b,_) -> compare a b) $ edges

mkLabels :: Ord a => [(a,b)] -> Labels a b
mkLabels labels = M.fromList labels

sampledat :: Edges Int
sampledat = mkEdges [
  (0,[2]),
  (1,[2]),
  (2,[3]),
  (4,[3]),
  (6,[3]),
  (3,[5])
  ]

samplelabels :: Labels Int String
samplelabels = mkLabels [
  (0,"l0"),
  (1,"l1"),
  (2,"l2"),
  (3,"l3"),
  (5,"l5"),
  (4,"l4"),
  (6,"l6")
  ]

-- | Grouping the nodes by the depth
--
-- >>> getDepthGroup sampledat
-- fromList [(0,[5]),(1,[3]),(2,[2,4,6]),(3,[0,1])]
getDepthGroup :: forall a. Ord a => Edges a -> DepthGroup a
getDepthGroup edges = M.fromList d2n
  where
    depth0 = getDepth2 edges
    depth1 = getDepth2 $ reverseEdges edges
    score :: a -> Depth
    score nodeid =
      maybe 0 id (M.lookup nodeid depth0) +
      maybe 0 id (M.lookup nodeid depth1)

    sort' :: S.Set a -> [a]
    sort' nodes = L.sortBy (\a b -> compare (score b) (score a)) $ S.toList nodes
    d2n = loop
          $ L.groupBy (\(a,_) (b,_) -> a == b)
          $ L.sortBy (\(a,_) (b,_) -> compare a b)
          $ map (\(a,b) -> (b,a))
          $ M.toList depth0
    loop :: [[(Depth,a)]] -> [(Depth,[a])]
    loop ls =
      case ls of
        [] -> []
        a:ax -> case a of
          (n,_):_ -> (n,sort' $ S.fromList $ map snd a): loop ax
          [] -> loop ax

-- | Grouping the nodes by the depth
--
-- >>> getDepthGroup2 samplelabels sampledat
-- fromList [(0,[5]),(1,[3]),(2,[2,4,6]),(3,[0,1])]
getDepthGroup2 :: forall a b. (Ord a,Ord b) => Labels a b -> Edges a -> DepthGroup a
getDepthGroup2 labels edges = M.fromList d2n
  where
    depth0 = getDepth2 edges
    depth1 = getDepth2 $ reverseEdges edges
    score :: a -> Depth
    score nodeid =
      maybe 0 id (M.lookup nodeid depth0) +
      maybe 0 id (M.lookup nodeid depth1)

    comp a b =
      case compare (score b) (score a) of
        EQ -> compare (M.lookup a labels) (M.lookup b labels)
        c -> c

    sort' :: S.Set a -> [a]
    sort' nodes = L.sortBy (\a b -> comp a b) $ S.toList nodes
    d2n = loop
          $ L.groupBy (\(a,_) (b,_) -> a == b)
          $ L.sortBy (\(a,_) (b,_) -> compare a b)
          $ map (\(a,b) -> (b,a))
          $ M.toList depth0
    loop :: [[(Depth,a)]] -> [(Depth,[a])]
    loop ls =
      case ls of
        [] -> []
        a:ax -> case a of
          (n,_):_ -> (n,sort' $ S.fromList $ map snd a): loop ax
          [] -> loop ax


getNodeDepth :: Ord a => DepthGroup a -> NodeDepth a
getNodeDepth dg = M.fromList $ concat $ map (\(d,nodes) -> map (\node -> (node,d)) nodes) $ M.toList dg


pairs :: M.Map b (S.Set a) -> [(a, b)]
pairs edges = do
  (p, c) <- M.toList edges
  child <- S.toList c
  return (child,p)

-- | Reverse the directions of edges
--
-- >>> sampledat
-- fromList [(0,fromList [2]),(1,fromList [2]),(2,fromList [3]),(3,fromList [5]),(4,fromList [3]),(6,fromList [3])]
-- >>> reverseEdges sampledat
-- fromList [(2,fromList [0,1]),(3,fromList [2,4,6]),(5,fromList [3])]
reverseEdges :: Ord a => Edges a -> Edges a
reverseEdges edges = M.fromList d2n
  where
    d2n = loop
          $ L.groupBy (\(a,_) (b,_) -> a == b)
          $ L.sortBy (\(a,_) (b,_) -> compare a b)
          $ pairs edges
    loop :: Ord a => [[(a,a)]] -> [(a,S.Set a)]
    loop ls =
      case ls of
        [] -> []
        a:ax -> case a of
          (n,_):_ -> (n,S.fromList $ map snd a): loop ax
          [] -> loop ax

-- | Get nodes by edges
--
-- >>> getNodes sampledat
-- fromList [0,1,2,3,4,5,6]
getNodes :: Ord a => Edges a -> Nodes a
getNodes edges = S.fromList $ parents ++ children
  where
    parents = do
      (parent, _) <- M.toList edges
      return parent
    children = do
      (_, c) <- M.toList edges
      child <- S.toList c
      return child


-- | Find all depth of nodes. This is faster than getDepth.
--
getDepth2 :: forall a. Ord a => Edges a -> DepthNode a
getDepth2 edges = runST $ do
  ref <- newSTRef M.empty
  mm <- forM (S.toList $ getNodes edges) $ \v -> do
     d <- getDepth2' ref v
     return (v,d)
  return $ M.fromList mm
  where
    getDepth2' :: STRef s (DepthNode a) -> a -> ST s Int
    getDepth2' ref i = do
      d <- readSTRef ref
      case M.lookup i d of
        Just v -> return v
        Nothing -> do
          case M.lookup i edges of
            Just v -> do
              dl <- forM (S.toList v) $ \v' -> do
                      getDepth2' ref v'
              let m = 1 + (maximum dl)
              d' <- readSTRef ref
              writeSTRef ref $ M.insert i m d'
              return m
            Nothing -> do
              writeSTRef ref $ M.insert i 0 d
              return 0

-- | Find all depth of nodes
--
getDepth :: forall a. Ord a => Edges a -> DepthNode a
getDepth edges = M.fromList $ map (\v -> (v,getDepth' v)) $ S.toList $ getNodes edges
  where
    getDepth' :: a -> Depth
    getDepth' i =
      case M.lookup i edges of
        Just v -> 1 + (maximum $ map (\v' -> getDepth' v') $ S.toList v)
        Nothing -> 0

-- | Move nodes to next step
--
-- >>> moveOne [(0,0,4)]
-- [((0,2,4),[(SRight,1)])]
-- >>> moveOne [(0,0,4),(0,4,0)]
-- [((0,2,4),[(SRight,1)]),((0,2,0),[(SLeft,3)])]
moveOne :: Ord a => [(a,Cur,Dest)] -> [((a,Cur,Dest),[(Symbol b,Pos)])]
moveOne nodes = do
  (n,c,g) <- nodes
  if c < g
  then
    return ((n,c+2,g),[(SRight,c+1)])
  else
    if c > g
      then
        return ((n,c-2,g),[(SLeft,c-1)])
      else
        return ((n,c,g),[(SHold,c)])

takeNode :: Ord a => Cur -> [((a,Cur,Dest),[(Symbol b,Pos)])] -> Maybe ((a,Cur,Dest),[(Symbol b,Pos)])
takeNode c nodes = L.find (\(_,syms) -> any (== c) (map snd syms)) nodes

-- | Move more nodes
--
-- >>> moveLeft' [((0,0,0),[(SHold,0)]),((1,0,0),[(SLeft,1)]),((2,2,0),[(SLeft,3)])]
-- [((0,0,0),[(SHold,0)]),((1,0,0),[(SLeft,1)]),((2,0,0),[(SLMove,2),(SLeft,3)])]
-- >>> moveLeft' [((0,0,0),[(SHold,0)]),((1,0,0),[(SLeft,1)]),((2,0,0),[(SLMove,2),(SLeft,3)]),((3,2,0),[(SLMove,4),(SLeft,5)])]
-- [((0,0,0),[(SHold,0)]),((1,0,0),[(SLeft,1)]),((2,0,0),[(SLMove,2),(SLeft,3)]),((3,0,0),[(SLMove,4),(SLeft,5)])]
-- >>> moveLeft' [((0,2,0),[(SLeft,3)])]
-- [((0,0,0),[(SLMove,1),(SLMove,2),(SLeft,3)])]
moveLeft' :: Ord a => [((a,Cur,Dest),[(Symbol b,Pos)])] -> [((a,Cur,Dest),[(Symbol b,Pos)])]
moveLeft' nodes = do
  nn@((n,c,g),syms) <- nodes
  if c > g
  then
    case (takeNode c nodes,takeNode (c-1) nodes) of
      (Nothing,Nothing) -> return ((n,c-2,g),((SLMove,c-1):(SLMove,c):syms))
      (Nothing,Just ((_,_,g'),_)) -> if g' == g then return ((n,c-2,g),((SLMove,c):syms)) else return nn
      (Just ((_,_,g'),_),_) -> if g' == g then return ((n,c-2,g),syms) else return nn
  else
    return nn

-- | Move more nodes
--
-- >>> moveLeft [((0,0,0),[(SHold,0)]),((1,0,0),[(SLeft,1)]),((2,2,0),[(SLeft,3)]),((3,4,0),[(SLeft,5)])]
-- [((0,0,0),[(SHold,0)]),((1,0,0),[(SLeft,1)]),((2,0,0),[(SLMove,2),(SLeft,3)]),((3,0,0),[(SLMove,4),(SLeft,5)])]
moveLeft :: (Ord a, Eq b) => [((a,Cur,Dest),[(Symbol b,Pos)])] -> [((a,Cur,Dest),[(Symbol b,Pos)])]
moveLeft nodes = if nodes == m then nodes else moveLeft m
  where
    m = moveLeft' nodes

-- | Move nodes to the next depth
--
-- >>> moveAll' [(0,0,4)] []
-- [[(SRight,1)],[(SRight,3)]]
-- >>> moveAll' [(0,4,0)] []
-- [[(SLMove,1),(SLMove,2),(SLeft,3)]]
-- >>> moveAll' [(0,2,0)] []
-- [[(SLeft,1)]]
-- >>> moveAll' [(0,0,4),(0,4,0)] []
-- [[(SRight,1),(SLeft,3)],[(SRight,3),(SLeft,1)]]
-- >>> moveAll' [(0,0,4),(0,2,0)] []
-- [[(SRight,1),(SLeft,1)],[(SRight,3),(SHold,0)]]
moveAll' :: (Ord a,Eq b) => [(a,Cur,Dest)] -> [[(Symbol b,Pos)]] -> [[(Symbol b,Pos)]]
moveAll' nodes buf | all (\(_,c,g) -> c==g) nodes && buf /= [] = buf
                   | otherwise = moveAll' (map fst next) (buf ++ [concat $ map snd next])
                      where
                        next = moveLeft $ moveOne nodes

mergeSymbol :: [(Symbol b,Pos)] -> [(Symbol b,Pos)]
mergeSymbol symbols =
  map (\v -> (foldl mappend mempty (map fst v),(snd (head v))))
  $ L.groupBy (\(_,p0) (_,p1) -> p0 == p1)
  $ L.sortBy (\(_,p0) (_,p1) -> p0 `compare` p1) symbols

-- | Fill spaces
--
-- >>> withSpace [(SRight,1),(SLeft,3)]
-- [SSpace,SRight,SSpace,SLeft]
-- >>> withSpace [(SRight,3),(SLeft,1)]
-- [SSpace,SLeft,SSpace,SRight]
withSpace :: [(Symbol b,Pos)] -> [Symbol b]
withSpace syms = merge sorted [0..max']
  where
    merge [] _ = []
    merge _ [] = []
    merge s@((s0,p0):sx) (p:px) | p0 == p = s0:merge sx px
                                | p0 <  p = merge sx (p:px)
                                | otherwise = SSpace:merge s px
    sorted = L.sortBy (\a b -> snd a `compare` snd b) syms
    max' = maximum $ map snd sorted

-- | Move nodes and fill spaces
--
-- >>> moveAllWithSpace [(0,0,4)]
-- [[SSpace,SRight],[SSpace,SSpace,SSpace,SRight]]
-- >>> moveAllWithSpace [(0,4,0)]
-- [[SSpace,SLMove,SLMove,SLeft]]
-- >>> moveAllWithSpace [(0,0,4),(0,4,0)]
-- [[SSpace,SRight,SSpace,SLeft],[SSpace,SLeft,SSpace,SRight]]
-- >>> moveAllWithSpace [(0,4,0),(1,0,4)]
-- [[SSpace,SRight,SSpace,SLeft],[SSpace,SLeft,SSpace,SRight]]
moveAllWithSpace :: (Ord a, Eq b) => [(a,Cur,Dest)] -> [[Symbol b]]
moveAllWithSpace nodes = map withSpace $ map mergeSymbol $ moveAll' nodes []

lstr :: (Ord a, Monoid b) => Labels a b -> a -> b
lstr labels nodeid = maybe mempty id (M.lookup nodeid labels)

nodeWithSpace :: (Ord a, Monoid b) => Labels a b -> ([(a,Cur,Dest)],[(a,Cur,Dest)]) -> [Symbol b]
nodeWithSpace labels (nodes,skipnodes) =
  withSpace $
    (map (\(nid,c,_) -> (SNode (lstr labels nid),c)) nodes) ++
    (map (\(_,c,_) -> (SHold,c)) skipnodes)

-- | Add bypass nodes
--
-- >>> let edges = mkEdges [(0,[1,2]),(1,[2])]
-- >>> let nd = getNodeDepth $ getDepthGroup edges
-- >>> addBypassNode'' 2 edges nd (M.fromList [(0,([2],[])),(1,([1],[])),(2,([0],[]))])
-- fromList [(0,([2],[])),(1,([1],[0])),(2,([0],[]))]
-- >>> let edges = mkEdges [(0,[1,3]),(1,[2]),(2,[3])]
-- >>> let nd = getNodeDepth $ getDepthGroup edges
-- >>> addBypassNode'' 3 edges nd (M.fromList [(0,([3],[])),(1,([2],[])),(2,([1],[])),(3,([0],[]))])
-- fromList [(0,([3],[])),(1,([2],[])),(2,([1],[0])),(3,([0],[]))]
-- >>> addBypassNode'' 2 edges nd (M.fromList [(0,([3],[])),(1,([2],[])),(2,([1],[0])),(3,([0],[]))])
-- fromList [(0,([3],[])),(1,([2],[0])),(2,([1],[0])),(3,([0],[]))]
--
-- >>> let edges = mkEdges [(0,[1,2]),(1,[4]),(2,[3]),(3,[4])]
-- >>> let nd = getNodeDepth $ getDepthGroup edges
-- >>> addBypassNode'' 2 edges nd (M.fromList [(0,([4],[])),(1,([3,1],[])),(2,([2],[0])),(3,([0],[]))])
-- fromList [(0,([4],[])),(1,([3,1],[])),(2,([2],[0])),(3,([0],[]))]
addBypassNode'' :: forall a. Ord a => Depth -> Edges a -> NodeDepth a -> DepthGroup' a -> DepthGroup' a
addBypassNode'' d edges nd dg | d < 2 = error $ "depth " ++ show d  ++ " must be greater than 2"
                              | otherwise =
  case (M.lookup d dg,M.lookup (d-1) dg) of
    (Just (nids0,skipnids0),Just n1@(nids1,_)) -> M.update (\_ -> Just (foldl (\n1' nid -> update nids1 n1' nid) n1 (nids0++skipnids0))) (d-1) dg
    _ -> dg
   where
    nodeDepth nid = maybe 0 id $ M.lookup nid nd
    edges' = M.fromList $ map (\(n,nids) ->  (n, S.fromList (filter (\nid -> nodeDepth nid < d) (S.toList nids)))) $ M.toList edges
    elem' :: Ord a => a -> [a] -> Bool
    elem' nid nids =
      case M.lookup nid edges' of
        Just m -> all id $ map (\n -> L.elem n nids) $ (S.toList m)
        Nothing -> True
    update :: Ord a => [a] -> ([a],[a]) -> a -> ([a],[a])
    update nids1 (v,skip) nid0 =
      if not (elem' nid0 nids1)
      then (v,skip++[nid0])
      else (v,skip)


-- | Get a maximum of depth
--
-- >>> maxDepth (M.fromList [(0,([2],[])),(1,([1],[])),(2,([0],[]))])
-- 2
maxDepth :: Ord a => DepthGroup' a -> Int
maxDepth dg = maximum $ map fst $ M.toList dg

-- | Add bypass nodes
--
-- >>> let edges = mkEdges [(0,[1,2]),(1,[2])]
-- >>> let nd = getNodeDepth $ getDepthGroup edges
-- >>> addBypassNode' edges nd (M.fromList [(0,([2],[])),(1,([1],[])),(2,([0],[]))])
-- fromList [(0,([2],[])),(1,([1],[0])),(2,([0],[]))]
-- >>> let edges = mkEdges [(0,[1,3]),(1,[2]),(2,[3])]
-- >>> let nd = getNodeDepth $ getDepthGroup edges
-- >>> addBypassNode' edges nd (M.fromList [(0,([3],[])),(1,([2],[])),(2,([1],[])),(3,([0],[]))])
-- fromList [(0,([3],[])),(1,([2],[0])),(2,([1],[0])),(3,([0],[]))]
addBypassNode' :: Ord a => Edges a -> NodeDepth a -> DepthGroup' a -> DepthGroup' a
addBypassNode' edges nd dg = foldr (\d dg' -> addBypassNode'' d edges nd dg') dg $ [2..(maxDepth dg)]

-- | Add bypass nodes
--
-- >>> let edges = mkEdges [(0,[1,2]),(1,[2])]
-- >>> let dg = getDepthGroup edges
-- >>> let nd = getNodeDepth dg
-- >>> addBypassNode edges nd dg
-- fromList [(0,([2],[])),(1,([1],[0])),(2,([0],[]))]
-- >>> let edges = mkEdges [(0,[1,3]),(1,[2]),(2,[3])]
-- >>> let dg = getDepthGroup edges
-- >>> let nd = getNodeDepth dg
-- >>> addBypassNode edges nd dg
-- fromList [(0,([3],[])),(1,([2],[0])),(2,([1],[0])),(3,([0],[]))]
-- >>> let edges = mkEdges [(0,[1,2]),(1,[4]),(2,[3]),(3,[4])]
-- >>> let dg = getDepthGroup edges
-- >>> let nd = getNodeDepth dg
-- >>> addBypassNode edges nd dg
-- fromList [(0,([4],[])),(1,([3,1],[])),(2,([2],[0])),(3,([0],[]))]
addBypassNode :: Ord a => Edges a -> NodeDepth a -> DepthGroup a -> DepthGroup' a
addBypassNode edges nd dg = addBypassNode' edges nd $ M.fromList $ map (\(k,v)-> (k,(v,[]))) $ M.toList dg

-- | Add destinations of nodes
--
-- >>> let edges = mkEdges [(0,[1,2]),(1,[2])]
-- >>> let dg = getDepthGroup edges
-- >>> addDestWithBypass edges $ M.fromList [(0,([2],[])),(1,([1],[0])),(2,([0],[]))]
-- fromList [(0,([(2,0,0)],[])),(1,([(1,0,0)],[(0,2,0)])),(2,([(0,0,0),(0,0,2)],[]))]
addDestWithBypass :: forall a. Ord a => Edges a -> DepthGroup' a -> DepthGroup'' a
addDestWithBypass edges dg = M.fromList $ mapAddPos $ reverse $ M.toList dg
  where
    mapAddPos :: Ord a => [(Int,([a],[a]))] -> [(Int,([(a,Cur,Dest)],[(a,Cur,Dest)]))]
    mapAddPos [] = []
    mapAddPos ((k,(a0,a1)):[]) = [(k,(zip3 a0 initpos0 initpos0,zip3 a1 initpos1 initpos1))]
      where
        initpos0 = map (*2) [0..]
        initpos1 = map (*2) [(length a0)..]
    mapAddPos ((ka,a):(kb,b):bx) = (ka,addDest edges a b): mapAddPos ((kb,b):bx)

-- | Grouping the nodes by the depth
--
-- >>> let edges = mkEdges [(0,[1,2])]
-- >>> let dg = getDepthGroup edges
-- >>> let nd = getNodeDepth dg
-- >>> dg
-- fromList [(0,[1,2]),(1,[0])]
-- >>> addNode edges nd dg
-- fromList [(0,([(1,0,0),(2,2,2)],[])),(1,([(0,0,0),(0,0,2)],[]))]
addNode :: Ord a => Edges a -> NodeDepth a -> DepthGroup a -> DepthGroup'' a
addNode edges nd dg = addDestWithBypass edges $ addBypassNode edges nd dg

toSymbol :: (Ord a, Eq b, Monoid b) => Labels a b -> DepthGroup'' a -> [[Symbol b]]
toSymbol labels dg = concat $ map (\(_,(n,s)) -> (nodeWithSpace labels (n,s)):moveAllWithSpace (n `mappend` s) ) $ reverse $ M.toList dg

edgesToText :: (Ord a) => Labels a String -> Edges a -> String
edgesToText labels edges = renderToText ( reverse $ drop 1 $ reverse $ toSymbol labels $ addNode edges nd dg) []
  where
    dg = getDepthGroup2 labels edges
    nd = getNodeDepth dg

symbolToChar :: Symbol b -> Char
symbolToChar (SNode _) = 'o'
symbolToChar SLeft = '/'
symbolToChar SRight = '\\'
symbolToChar SHold = '|'
symbolToChar SCross = 'x'
symbolToChar SLMove = '_'
symbolToChar SRMove = '_'
symbolToChar SSpace = ' '


-- | Rendering symbols to text
--
-- >>> renderToText [[SNode ""],[SHold],[SNode ""]] []
-- "o\n|\no\n"
-- >>> renderToText [[SNode "",SSpace,SNode ""],[SHold,SLeft],[SNode ""]] []
-- "o o\n|/\no\n"
renderToText :: [[Symbol String]] -> [String] -> String
renderToText [] _ = []
renderToText ([]:sxx) labelbuf = (if 0 == foldr (\i s -> s + length i) 0 labelbuf
                                  then ""
                                  else if len >= 4 && llen >= 2
                                       then str0
                                       else str
                                 ) `mappend` "\n" `mappend` renderToText sxx []
  where
    str = "    " `mappend` (L.intercalate "," labelbuf)
    str0 = "    " `mappend` prefix `mappend` "{" `mappend` (L.intercalate "," (map (drop len) labelbuf)) `mappend` "}"
    prefix = getLongestCommonPrefix labelbuf
    len = length prefix
    llen = length labelbuf

renderToText ((s@(SNode label):sx):sxx) labelbuf = (symbolToChar s):(renderToText (sx:sxx) (labelbuf `mappend` [label]))
renderToText ((s:sx):sxx) labelbuf = (symbolToChar s):(renderToText (sx:sxx) labelbuf)

getLongestCommonPrefix :: [String] -> String
getLongestCommonPrefix [] = []
getLongestCommonPrefix (str:strs) = foldl (\a b -> getLongestCommonPrefix' a b []) str strs
  where
    getLongestCommonPrefix' :: String -> String -> String -> String
    getLongestCommonPrefix' (x:xs) (y:ys) buf | x == y = getLongestCommonPrefix' xs ys (buf ++ (x:[]))
                                              | otherwise = buf
    getLongestCommonPrefix' [] _ buf = buf
    getLongestCommonPrefix' _ [] buf = buf

-- | Allocate destinations of nodes.
--
-- >>> addDest sampledat ([0,1],[]) ([2],[])
-- ([(0,0,0),(1,2,0)],[])
-- >>> addDest (mkEdges [(0,[1,2]),(1,[2])]) ([0],[]) ([1],[0])
-- ([(0,0,0),(0,0,2)],[])
-- >>> addDest (mkEdges [(0,[1,2]),(1,[2])]) ([1],[0]) ([2],[])
-- ([(1,0,0)],[(0,2,0)])
-- >>> addDest (mkEdges [(0,[1,3]),(1,[2]),(2,[3])]) ([1],[0]) ([2],[0])
-- ([(1,0,0)],[(0,2,2)])
addDest :: Ord a => Edges a -> ([a],[a]) -> ([a],[a]) -> ([(a,Cur,Dest)],[(a,Cur,Dest)])
addDest edges (curn,curs) (nxtn,nxts) = (n2n++n2s,s2n++s2s)
  where
    curn' = zip curn $ map (*2) [0..]
    curs' = zip curs $ map (*2) [(length curn)..]
    nxtn' = zip nxtn $ map (*2) [0..]
    nxts' = zip nxts $ map (*2) [(length nxtn)..]
    n2s = concat $ flip map curn' $ \(c,i) ->
            case L.find (\(nid,_) -> nid == c) nxts' of
              Just (_,ii) -> [(c,i,ii)]
              Nothing -> []
    s2s = concat $ flip map curs' $ \(c,i) ->
            case L.find (\(nid,_) -> nid == c) nxts' of
              Just (_,ii) -> [(c,i,ii)]
              Nothing -> []
    n2n = concat $ flip map curn' $ \(c,i) ->
            case M.lookup c edges of
              Just c' -> concat $ flip map (S.toList c') $ \c'' ->
                case L.find (\(nid,_) -> nid == c'') nxtn' of
                  Just (_,ii) -> [(c,i,ii)]
                  Nothing -> []
              Nothing -> []
    s2n = concat $ flip map curs' $ \(c,i) ->
            case M.lookup c edges of
              Just c' -> concat $ flip map (S.toList c') $ \c'' ->
                case L.find (\(nid,_) -> nid == c'') nxtn' of
                  Just (_,ii) -> [(c,i,ii)]
                  Nothing -> []
              Nothing -> []
