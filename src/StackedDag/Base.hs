module StackedDag.Base where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Maybe(maybe)

type NodeId = Int

type Edges = M.Map NodeId (S.Set NodeId)

type Labels = M.Map NodeId String

data Symbol =
    SNode String -- o with label
  | SLeft -- /
  | SRight -- \
  | SHold -- |
  | SLMove -- _
  | SRMove -- _
  | SCross -- x
  | SSpace -- ' '
  deriving (Show, Read, Eq)

instance Semigroup Symbol where
  (<>) n@(SNode _) _  = n
  (<>) _ n@(SNode _) = n
  (<>) a SSpace = a
  (<>) SSpace a = a
  (<>) SLeft SRight = SCross
  (<>) SRight SLeft = SCross
  (<>) SCross SRight = SCross
  (<>) SCross SLeft = SCross
  (<>) SRight SCross = SCross
  (<>) SLeft SCross = SCross
  (<>) a _ = a


instance Monoid Symbol where
  mempty = SSpace

type Nodes = S.Set NodeId

type Depth = Int
type Dest = Int
type Cur = Int
type Pos = Int

type DepthNode = M.Map NodeId Depth

type DepthGroup = M.Map Depth [NodeId]

type NodeDepth = M.Map NodeId Depth

type DepthGroup' = M.Map Depth ([NodeId],[NodeId])

type DepthGroup'' = M.Map Depth ([(NodeId,Cur,Dest)],[(NodeId,Cur,Dest)])

mkEdges :: [(NodeId,[NodeId])] -> Edges
mkEdges edges = M.fromList $ map (\(nid,nids) -> (nid, S.fromList nids)) g
  where
    g = map (\xs@((k,_):_)-> (k,concat $ map snd xs) ) $ L.groupBy (\(a,_) (b,_) -> a == b) $  L.sortBy (\(a,_) (b,_) -> compare a b) $ edges

mkLabels ::[(NodeId,String)] -> Labels
mkLabels labels = M.fromList labels

sampledat :: Edges
sampledat = mkEdges [
  (0,[2]),
  (1,[2]),
  (2,[3]),
  (4,[3]),
  (6,[3]),
  (3,[5])
  ]

samplelabels :: Labels
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
getDepthGroup :: Edges -> DepthGroup
getDepthGroup edges = M.fromList d2n
  where
    depth0 = getDepth2 edges
    depth1 = getDepth2 $ reverseEdges edges
    score nodeid =
      maybe 0 id (M.lookup nodeid depth0) +
      maybe 0 id (M.lookup nodeid depth1)

    sort' :: S.Set NodeId -> [NodeId]
    sort' nodes = L.sortBy (\a b -> compare (score b) (score a)) $ S.toList nodes

    d2n = loop
          $ L.groupBy (\(a,_) (b,_) -> a == b)
          $ L.sortBy (\(a,_) (b,_) -> compare a b)
          $ map (\(a,b) -> (b,a))
          $ M.toList depth0
    loop :: [[(NodeId,Depth)]] -> [(Depth,[NodeId])]
    loop ls =
      case ls of
        [] -> []
        a:ax -> case a of
          (n,d):_ -> (n,sort' $ S.fromList $ map snd a): loop ax
          [] -> loop ax

getNodeDepth :: DepthGroup -> NodeDepth
getNodeDepth dg = M.fromList $ concat $ map (\(d,nodes) -> map (\node -> (node,d)) nodes) $ M.toList dg


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
reverseEdges :: Edges -> Edges
reverseEdges edges = M.fromList d2n
  where
    d2n = loop
          $ L.groupBy (\(a,_) (b,_) -> a == b)
          $ L.sortBy (\(a,_) (b,_) -> compare a b)
          $ pairs edges
    loop :: [[(NodeId,NodeId)]] -> [(NodeId,S.Set NodeId)]
    loop ls =
      case ls of
        [] -> []
        a:ax -> case a of
          (n,d):_ -> (n,S.fromList $ map snd a): loop ax
          [] -> loop ax

-- | Get nodes by edges
--
-- >>> getNodes sampledat
-- fromList [0,1,2,3,4,5,6]
getNodes :: Edges -> Nodes
getNodes edges = S.fromList $ parents ++ children
  where
    parents = do
      (parent, c) <- M.toList edges
      return parent
    children = do
      (_, c) <- M.toList edges
      child <- S.toList c
      return child


getDepth2 :: Edges -> DepthNode
getDepth2 edges = runST $ do
  ref <- newSTRef M.empty
  mm <- forM (S.toList $ getNodes edges) $ \v -> do
     d <- getDepth2' ref v edges
     return (v,d)
  return $ M.fromList mm

getDepth2' :: STRef s DepthNode -> Int -> Edges -> ST s Int
getDepth2' ref i edges = do
  d <- readSTRef ref
  case M.lookup i d of
    Just v -> return v
    Nothing -> do
      case M.lookup i edges of
        Just v -> do
          dl <- forM (S.toList v) $ \v' -> do
                  getDepth2' ref v' edges
          let m = 1 + (maximum dl)
          d' <- readSTRef ref
          writeSTRef ref $ M.insert i m d'
          return m
        Nothing -> do
          writeSTRef ref $ M.insert i 0 d
          return 0


{-
getDepth :: Edges -> DepthNode
getDepth edges = M.fromList $ map (\v -> (v,getDepth' v edges)) $ S.toList $ getNodes edges

getDepth' :: Int -> Edges -> Int
getDepth' i edges =
  case M.lookup i edges of
    Just v -> 1 + (maximum $ map (\v' -> getDepth' v' edges ) $ S.toList v)
    Nothing -> 0
-}

-- | Move nodes to next step
--
-- >>> moveOne [(0,0,4)]
-- [((0,2,4),[(SRight,1)])]
-- >>> moveOne [(0,0,4),(0,4,0)]
-- [((0,2,4),[(SRight,1)]),((0,2,0),[(SLeft,3)])]
moveOne :: [(NodeId,Cur,Dest)] -> [((NodeId,Cur,Dest),[(Symbol,Pos)])]
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

takeNode :: Cur -> [((NodeId,Cur,Dest),[(Symbol,Pos)])] -> Maybe ((NodeId,Cur,Dest),[(Symbol,Pos)])
takeNode c nodes = L.find (\(_,syms) -> any (== c) (map snd syms)) nodes

-- | Move more nodes
--
-- >>> moveLeft' [((0,0,0),[(SHold,0)]),((1,0,0),[(SLeft,1)]),((2,2,0),[(SLeft,3)])]
-- [((0,0,0),[(SHold,0)]),((1,0,0),[(SLeft,1)]),((2,0,0),[(SLMove,2),(SLeft,3)])]
-- >>> moveLeft' [((0,0,0),[(SHold,0)]),((1,0,0),[(SLeft,1)]),((2,0,0),[(SLMove,2),(SLeft,3)]),((3,2,0),[(SLMove,4),(SLeft,5)])]
-- [((0,0,0),[(SHold,0)]),((1,0,0),[(SLeft,1)]),((2,0,0),[(SLMove,2),(SLeft,3)]),((3,0,0),[(SLMove,4),(SLeft,5)])]
-- >>> moveLeft' [((0,2,0),[(SLeft,3)])]
-- [((0,0,0),[(SLMove,1),(SLMove,2),(SLeft,3)])]
moveLeft' :: [((NodeId,Cur,Dest),[(Symbol,Pos)])] -> [((NodeId,Cur,Dest),[(Symbol,Pos)])]
moveLeft' nodes = do
  nn@((n,c,g),syms) <- nodes
  if c > g
  then
    case (takeNode c nodes,takeNode (c-1) nodes) of
      (Nothing,Nothing) -> return ((n,c-2,g),((SLMove,c-1):(SLMove,c):syms))
      (Nothing,Just ((n',c',g'),_)) -> if g' == g then return ((n,c-2,g),((SLMove,c):syms)) else return nn
      (Just ((_,_,g'),_),_) -> if g' == g then return ((n,c-2,g),syms) else return nn
  else
    return nn

-- | Move more nodes
--
-- >>> moveLeft [((0,0,0),[(SHold,0)]),((1,0,0),[(SLeft,1)]),((2,2,0),[(SLeft,3)]),((3,4,0),[(SLeft,5)])]
-- [((0,0,0),[(SHold,0)]),((1,0,0),[(SLeft,1)]),((2,0,0),[(SLMove,2),(SLeft,3)]),((3,0,0),[(SLMove,4),(SLeft,5)])]
moveLeft :: [((NodeId,Cur,Dest),[(Symbol,Pos)])] -> [((NodeId,Cur,Dest),[(Symbol,Pos)])]
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
moveAll' :: [(NodeId,Cur,Dest)] -> [[(Symbol,Pos)]] -> [[(Symbol,Pos)]]
moveAll' nodes buf | all (\(_,c,g) -> c==g) nodes && buf /= [] = buf
                   | otherwise = moveAll' (map fst next) (buf ++ [concat $ map snd next])
                      where
                        next = moveLeft $ moveOne nodes

mergeSymbol :: [(Symbol,Pos)] -> [(Symbol,Pos)]
mergeSymbol symbols =
  map (\v -> (foldl mappend mempty (map fst v),(snd (head v))))
  $ L.groupBy (\(s0,p0) (s1,p1) -> p0 == p1)
  $ L.sortBy (\(s0,p0) (s1,p1) -> p0 `compare` p1) symbols

-- | Fill spaces
--
-- >>> withSpace [(SRight,1),(SLeft,3)]
-- [(SSpace,0),(SRight,1),(SSpace,2),(SLeft,3)]
-- >>> withSpace [(SRight,3),(SLeft,1)]
-- [(SSpace,0),(SLeft,1),(SSpace,2),(SRight,3)]
withSpace :: [(Symbol,Pos)] -> [(Symbol,Pos)]
withSpace syms = merge sorted [0..max]
  where
    merge [] _ = []
    merge _ [] = []
    merge s@((s0,p0):sx) (p:px) | p0 == p = (s0,p0):merge sx px
                                | p0 <  p = merge sx (p:px)
                                | otherwise = (SSpace,p):merge s px
    sorted = L.sortBy (\a b -> snd a `compare` snd b) syms
    max = maximum $ map snd sorted

-- | Move nodes and fill spaces
--
-- >>> moveAllWithSpace [(0,0,4)]
-- [[(SSpace,0),(SRight,1)],[(SSpace,0),(SSpace,1),(SSpace,2),(SRight,3)]]
-- >>> moveAllWithSpace [(0,4,0)]
-- [[(SSpace,0),(SLMove,1),(SLMove,2),(SLeft,3)]]
-- >>> moveAllWithSpace [(0,0,4),(0,4,0)]
-- [[(SSpace,0),(SRight,1),(SSpace,2),(SLeft,3)],[(SSpace,0),(SLeft,1),(SSpace,2),(SRight,3)]]
-- >>> moveAllWithSpace [(0,4,0),(1,0,4)]
-- [[(SSpace,0),(SRight,1),(SSpace,2),(SLeft,3)],[(SSpace,0),(SLeft,1),(SSpace,2),(SRight,3)]]
moveAllWithSpace :: [(NodeId,Cur,Dest)] -> [[(Symbol,Pos)]]
moveAllWithSpace nodes = map withSpace $ map mergeSymbol $ moveAll' nodes []

lstr :: Labels -> NodeId -> String
lstr labels nodeid = maybe "" id (M.lookup nodeid labels)

nodeWithSpace :: Labels -> ([(NodeId,Cur,Dest)],[(NodeId,Cur,Dest)]) -> [(Symbol,Pos)]
nodeWithSpace labels (nodes,skipnodes) =
  withSpace $
    (map (\(nid,c,_) -> (SNode (lstr labels nid),c)) nodes) ++
    (map (\(_,c,_) -> (SHold,c)) skipnodes)

-- | Add bypass nodes
--
-- >>> edges = mkEdges [(0,[1,2]),(1,[2])]
-- >>> addBypassNode'' 2 edges (M.fromList [(0,([2],[])),(1,([1],[])),(2,([0],[]))])
-- fromList [(0,([2],[])),(1,([1],[0])),(2,([0],[]))]
-- >>> edges = mkEdges [(0,[1,3]),(1,[2]),(2,[3])]
-- >>> addBypassNode'' 3 edges (M.fromList [(0,([3],[])),(1,([2],[])),(2,([1],[])),(3,([0],[]))])
-- fromList [(0,([3],[])),(1,([2],[])),(2,([1],[0])),(3,([0],[]))]
-- >>> addBypassNode'' 2 edges (M.fromList [(0,([3],[])),(1,([2],[])),(2,([1],[0])),(3,([0],[]))])
-- fromList [(0,([3],[])),(1,([2],[0])),(2,([1],[0])),(3,([0],[]))]
--
-- >>> edges = mkEdges [(0,[1,2]),(1,[4]),(2,[3]),(3,[4])]
-- >>> addBypassNode'' 2 edges (M.fromList [(0,([4],[])),(1,([3,1],[])),(2,([2],[0])),(3,([0],[]))])
-- fromList [(0,([4],[])),(1,([3,1],[])),(2,([2],[0])),(3,([0],[]))]
addBypassNode'' :: Depth -> Edges -> DepthGroup' -> DepthGroup'
addBypassNode'' d edges dg | d < 2 = error $ "depth " ++ show d  ++ " must be greater than 2"
                           | otherwise =
  case (M.lookup d dg,M.lookup (d-1) dg) of
    (Just (nids0,skipnids0),Just n1@(nids1,v)) -> M.update (\_ -> Just (foldl (\n1' nid -> update nids1 n1' nid) n1 (nids0++skipnids0))) (d-1) dg
    (Just (nids0,skipnids0),Nothing)        -> dg
    (Nothing,_)                             -> dg
  where
    nd = getNodeDepth $ getDepthGroup edges
    getDepth :: NodeId -> Depth
    getDepth nid = maybe 0 id $ M.lookup nid nd
    edges' :: Edges
    edges' = M.fromList $ map (\(n,nids) ->  (n, S.fromList (filter (\nid -> getDepth nid < d) (S.toList nids)))) $ M.toList edges
    elem :: NodeId -> [NodeId] -> Bool
    elem nid nids =
      case M.lookup nid edges' of
        Just m -> all id $ map (\n -> L.elem n nids) $ (S.toList m)
        Nothing -> True
    update :: [NodeId] -> ([NodeId],[NodeId]) -> NodeId -> ([NodeId],[NodeId])
    update nids1 (v,skip) nid0 =
      if not (elem nid0 nids1)
      then (v,skip++[nid0])
      else (v,skip)


-- | Get a maximum of depth
--
-- >>> maxDepth (M.fromList [(0,([2],[])),(1,([1],[])),(2,([0],[]))])
-- 2
maxDepth :: DepthGroup' -> Int
maxDepth dg = maximum $ map fst $ M.toList dg

-- | Add bypass nodes
--
-- >>> edges = mkEdges [(0,[1,2]),(1,[2])]
-- >>> addBypassNode' edges (M.fromList [(0,([2],[])),(1,([1],[])),(2,([0],[]))])
-- fromList [(0,([2],[])),(1,([1],[0])),(2,([0],[]))]
-- >>> edges = mkEdges [(0,[1,3]),(1,[2]),(2,[3])]
-- >>> addBypassNode' edges (M.fromList [(0,([3],[])),(1,([2],[])),(2,([1],[])),(3,([0],[]))])
-- fromList [(0,([3],[])),(1,([2],[0])),(2,([1],[0])),(3,([0],[]))]
addBypassNode' :: Edges -> DepthGroup' -> DepthGroup'
addBypassNode' edges dg = foldr (\d dg' -> addBypassNode'' d edges dg') dg $ [2..(maxDepth dg)]

-- | Add bypass nodes
--
-- >>> edges = mkEdges [(0,[1,2]),(1,[2])]
-- >>> dg = getDepthGroup edges
-- >>> addBypassNode edges dg
-- fromList [(0,([2],[])),(1,([1],[0])),(2,([0],[]))]
-- >>> edges = mkEdges [(0,[1,3]),(1,[2]),(2,[3])]
-- >>> dg = getDepthGroup edges
-- >>> addBypassNode edges dg
-- fromList [(0,([3],[])),(1,([2],[0])),(2,([1],[0])),(3,([0],[]))]
-- >>> edges = mkEdges [(0,[1,2]),(1,[4]),(2,[3]),(3,[4])]
-- >>> dg = getDepthGroup edges
-- >>> addBypassNode edges dg
-- fromList [(0,([4],[])),(1,([3,1],[])),(2,([2],[0])),(3,([0],[]))]
addBypassNode :: Edges -> DepthGroup -> DepthGroup'
addBypassNode edges dg = addBypassNode' edges $ M.fromList $ map (\(k,v)-> (k,(v,[]))) $ M.toList dg

-- | Add destinations of nodes
--
-- >>> edges = mkEdges [(0,[1,2]),(1,[2])]
-- >>> dg = getDepthGroup edges
-- >>> addPosNode edges $ M.fromList [(0,([2],[])),(1,([1],[0])),(2,([0],[]))]
-- fromList [(0,([(2,0,0)],[])),(1,([(1,0,0)],[(0,2,0)])),(2,([(0,0,0),(0,0,2)],[]))]
addPosNode :: Edges -> DepthGroup' -> DepthGroup''
addPosNode edges dg = M.fromList $ mapAddPos $ reverse $ M.toList dg
  where
    mapAddPos :: [(Int,([NodeId],[NodeId]))] -> [(Int,([(NodeId,Cur,Dest)],[(NodeId,Cur,Dest)]))]
    mapAddPos [] = []
    mapAddPos ((k,(a0,a1)):[]) = [(k,(zip3 a0 initpos0 initpos0,zip3 a1 initpos1 initpos1))]
      where
        initpos0 = map (*2) [0..]
        initpos1 = map (*2) [(length a0)..]
    mapAddPos ((ka,a):(kb,b):bx) = (ka,addPos edges a b): mapAddPos ((kb,b):bx)

-- | Grouping the nodes by the depth
--
-- >>> edges = mkEdges [(0,[1,2])]
-- >>> dg = getDepthGroup edges
-- >>> dg
-- fromList [(0,[1,2]),(1,[0])]
-- >>> addNode edges dg
-- fromList [(0,([(1,0,0),(2,2,2)],[])),(1,([(0,0,0),(0,0,2)],[]))]
addNode :: Edges -> DepthGroup -> DepthGroup''
addNode edges dg = addPosNode edges $ addBypassNode edges dg

toSymbol :: Labels -> DepthGroup'' -> [[(Symbol,Pos)]]
toSymbol labels dg = concat $ map (\(k,(n,s)) -> (nodeWithSpace labels (n,s)):moveAllWithSpace (n++s) ) $ reverse $ M.toList dg

edgesToText :: Labels -> Edges -> String
edgesToText labels edges = renderToText ( reverse $ drop 1 $ reverse $ toSymbol labels $ addNode edges $ getDepthGroup edges) []

symbolToChar :: Symbol -> Char
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
-- >>> renderToText [[(SNode "",0)],[(SHold,0)],[(SNode "",0)]] []
-- "o\n|\no\n"
-- >>> renderToText [[(SNode "",0),(SSpace,1),(SNode "",2)],[(SHold,0),(SLeft,1)],[(SNode "",0)]] []
-- "o o\n|/\no\n"
renderToText :: [[(Symbol,Pos)]] -> [String] -> String
renderToText [] _ = []
renderToText ([]:sxx) labelbuf = (if 0 == foldr (\i s -> s + length i) 0 labelbuf
                                  then ""
                                  else if len >= 4 && llen >= 2
                                       then str0
                                       else str
                                 )++ "\n" ++ renderToText sxx []
  where
    str = "    " ++ (L.intercalate "," labelbuf)
    str0 = "    " ++ prefix ++ "{" ++ (L.intercalate "," (map (drop len) labelbuf)) ++ "}"
    prefix = getLongestCommonPrefix labelbuf
    len = length prefix
    llen = length labelbuf

renderToText ((s@(SNode label,_):sx):sxx) labelbuf = (symbolToChar (fst s)):(renderToText (sx:sxx) (labelbuf ++ [label]))
renderToText ((s:sx):sxx) labelbuf = (symbolToChar (fst s)):(renderToText (sx:sxx) labelbuf)

getLongestCommonPrefix' :: String -> String -> String -> String
getLongestCommonPrefix' (x:xs) (y:ys) buf | x == y = getLongestCommonPrefix' xs ys (buf ++ (x:[]))
                                          | otherwise = buf
getLongestCommonPrefix' [] _ buf = buf
getLongestCommonPrefix' _ [] buf = buf


getLongestCommonPrefix :: [String] -> String
getLongestCommonPrefix (str:strs) = foldl (\a b -> getLongestCommonPrefix' a b []) str strs
getLongestCommonPrefix [] = []


-- | Allocate destinations of nodes.
--
-- >>> addPos sampledat ([0,1],[]) ([2],[])
-- ([(0,0,0),(1,2,0)],[])
-- >>> addPos (mkEdges [(0,[1,2]),(1,[2])]) ([0],[]) ([1],[0])
-- ([(0,0,0),(0,0,2)],[])
-- >>> addPos (mkEdges [(0,[1,2]),(1,[2])]) ([1],[0]) ([2],[])
-- ([(1,0,0)],[(0,2,0)])
-- >>> addPos (mkEdges [(0,[1,3]),(1,[2]),(2,[3])]) ([1],[0]) ([2],[0])
-- ([(1,0,0)],[(0,2,2)])
addPos :: Edges -> ([NodeId],[NodeId]) -> ([NodeId],[NodeId]) -> ([(NodeId,Cur,Dest)],[(NodeId,Cur,Dest)])
addPos edges (curn,curs) (nxtn,nxts) = (n2n++n2s,s2n++s2s)
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
