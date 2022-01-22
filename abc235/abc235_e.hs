{-# LANGUAGE PackageImports #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad (replicateM)
import Data.List (unfoldr, find, delete)
import Data.Char (isSpace)
import Data.Tuple (swap)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set
import qualified Data.IntMap.Strict as IM
import qualified Data.HashMap.Strict as HM
import qualified "heaps" Data.Heap as Heap
import Debug.Trace (traceShow)

bsReadIntList :: IO [ Int ]
bsReadIntList = bsGetIntList <$> BS.getLine

bsGetIntList :: BS.ByteString -> [ Int ]
bsGetIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

listToTuple3 :: [ a ] -> (a, a, a)
listToTuple3 (x:y:z:_) = (x, y, z)

bsReadIntTuple3ListN :: Int -> IO [ (Int, Int, Int) ]
bsReadIntTuple3ListN = flip replicateM (listToTuple3 . bsGetIntList <$> BS.getLine)

main :: IO ()
main = do
  (_n:m:q:_) <- bsReadIntList
  graph <- bsReadIntTuple3ListN m
  query <- bsReadIntTuple3ListN q
  let t = mst graph
  mapM_ (printResult . queryT t) query

type Edge = (Int, Int, Int)
type AdjacencyList = IM.IntMap (IM.IntMap Int)
type AdjCache = Heap.Heap (Int, (Int, Int))

makeAdjacencyList :: [ Edge ] -> AdjacencyList
makeAdjacencyList = IM.fromListWith (<>) . foldr (\(v1,v2,w) acc -> (v1, IM.singleton v2 w):(v2, IM.singleton v1 w):acc) []

mst :: [ Edge ] -> [ Edge ]
mst graph@((v1,_,_):_) = do
  let adjList = makeAdjacencyList graph
  let adjCache = updateAdjCache adjList v1
  prim (Set.singleton v1) adjList adjCache []

prim :: Set.Set Int -> AdjacencyList -> AdjCache -> [ Edge ] -> [ Edge ]
prim known adjList adjCache res = if not . IM.null $ adjList then do
  let (adjCacheRest, edge, v) = searchInAdjCache known adjCache
  let knownNext = Set.insert v known
  let adjCacheNext = Heap.union (updateAdjCache adjList v) adjCacheRest
  let resNext = edge:res
  let adjListNext = updateAdjList adjList v known
  prim knownNext adjListNext adjCacheNext resNext
  else res

updateAdjList :: AdjacencyList -> Int -> Set.Set Int -> AdjacencyList
updateAdjList adjList v1 = Set.foldr (\v2 acc -> IM.update (updateAdj . IM.delete v1) v2 (IM.update (updateAdj . IM.delete v2) v1 acc)) adjList
  where
    updateAdj im = if IM.null im then Nothing else Just im

pair :: Int -> Int -> (Int, Int)
pair v1 v2 = (min v1 v2, max v1 v2)

updateAdjCache :: AdjacencyList -> Int -> AdjCache
updateAdjCache adjList v1 = case IM.lookup v1 adjList of
  Just vs -> Heap.fromList . foldr (\(v2, w) acc -> (w, pair v1 v2):acc) [] . IM.toList $ vs
  Nothing -> Heap.empty

searchInAdjCache :: Set.Set Int -> AdjCache -> (AdjCache, Edge, Int)
searchInAdjCache known adjList = case Heap.uncons adjList of
  Just ((w, (v1, v2)), rest) -> case getUnknown known v1 v2 of
    Nothing -> searchInAdjCache known rest
    Just v -> (rest, (v1, v2, w), v)
  Nothing -> error "empty heap"

getUnknown :: Set.Set Int -> Int -> Int -> Maybe Int
getUnknown known v1 v2
  | v1 == v2 = Just v1
  | Set.notMember v1 known = Just v1
  | Set.notMember v2 known = Just v2
  | otherwise = Nothing

replace :: [ Edge ] -> Edge -> Bool
replace t (va1, va2, wa) = case find (\(vb1, vb2, wb) -> va1 == vb1 && va2 == vb2 && wa < wb) t of
  Just _ -> True
  Nothing -> False

cycleDrop :: [ Edge ] -> Edge -> Bool
cycleDrop t edge@(_, _, w1) = do
  let cyc = rmForCycle (IM.fromListWith (<>) $ foldr (\e@(v1, v2, _w) acc -> (v1, [e]):(v2, [e]):acc) [] t)
  cycleMinW w1 . map (\(_,_,w) -> w) . delete edge $ cyc

cycleMinW :: Int -> [ Int ] -> Bool
cycleMinW _c [] = True
cycleMinW c (w:wrest) = c < foldr min w wrest

rmForCycle :: IM.IntMap [ Edge ] -> [ Edge ]
rmForCycle t = do
  let oldSize = IM.size t
  let rest = IM.filter (\es -> length es > 1) t
  let nextSize = IM.size rest
  if oldSize == nextSize then concatMap snd . IM.toList $ rest
    else rmForCycle rest

queryT :: [ Edge ] -> Edge -> Bool
queryT t edge = replace t edge || cycleDrop t edge

data Ans = Yes | No deriving (Show)

printResult :: Bool -> IO ()
printResult True = print Yes
printResult False = print No
