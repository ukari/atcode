{-# LANGUAGE PackageImports #-}

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
  let selfCyc = selfCycle graph
  mapM_ (printResult . queryT t selfCyc) query

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

selfCycle :: [ Edge ] -> [ Edge ]
selfCycle = filter (\(v1, v2, _w) -> v1 == v2)

rmForCycle :: IM.IntMap [ Edge ] -> [ Edge ]
rmForCycle t = do
  let oldSize = IM.size t
  let rest = IM.filter (\es -> length es > 1) t
  let nextSize = IM.size rest
  if oldSize == nextSize then concatMap snd . IM.toList $ rest
    else rmForCycle rest

queryT :: [ Edge ] -> [ Edge ]-> Edge -> Bool
queryT t selfCyc edge = if isSelfCycleEdge edge then checkSelfCycle selfCyc edge else replace t edge || cycleDrop t edge

isSelfCycleEdge :: Edge -> Bool
isSelfCycleEdge (v1, v2, _w) = v1 == v2

checkSelfCycle :: [ Edge ] -> Edge -> Bool
checkSelfCycle selfCyc (va1, va2, wa) = cycleMinW wa . map (\(_,_,w) -> w) . filter (\(vb1, vb2, _wb) -> va1 == vb1 && va2 == vb1) $ selfCyc

data Ans = Yes | No deriving (Show)

printResult :: Bool -> IO ()
printResult True = print Yes
printResult False = print No

type Edge = (Int, Int, Int)
type EdgeCache = HM.HashMap (Int, Int) Int 
type ConnectEdgeCache = Heap.Heap (Heap.Entry Int (Int, Int))

tupleToEntry :: (Int, (Int, Int)) -> Heap.Entry Int (Int, Int)
tupleToEntry (w, vp) = Heap.Entry w vp

entryToTuple :: Heap.Entry Int (Int, Int) -> (Int, Int, Int)
entryToTuple (Heap.Entry w (v1, v2)) = (v1, v2, w)

toCache :: Edge -> ((Int, Int), Int)
toCache (v1, v2, w) = ((v1, v2), w)

edgeCacheToConnectEdgeCache :: EdgeCache -> ConnectEdgeCache
edgeCacheToConnectEdgeCache = Heap.fromList . map (tupleToEntry . swap) . HM.toList

mst :: [ Edge ] -> [ Edge ]
mst [] = []
mst graph@((v, _, _):_) = do
  let edgeCache = HM.fromListWith min . map toCache $ graph
  let known = Set.singleton v
  let connectCache = getAdjs v edgeCache Heap.empty
  let unknown = Set.delete v . Set.fromList . getVs $ graph
  prim edgeCache known unknown connectCache []

getVs :: [ Edge ] -> [ Int ]
getVs = foldr (\(v1, v2, _w) acc-> v1:v2:acc) []

getAdjs :: Int -> EdgeCache -> ConnectEdgeCache -> ConnectEdgeCache
getAdjs addV edgeCache connectCache = Heap.union connectCache . edgeCacheToConnectEdgeCache . HM.filterWithKey (\(v1, v2) _w -> v1 == addV || v2 == addV) $ edgeCache

removeKnown :: EdgeCache -> Set.Set Int -> Int -> EdgeCache
removeKnown edgeCache known addv = foldr (HM.delete . makePair addv) edgeCache . Set.toList $ known

makePair :: Int -> Int -> (Int, Int)
makePair v1 v2 = (min v1 v2, max v1 v2)

queryUnknown :: (Int, Int) -> Set.Set Int -> Int
queryUnknown (v1, v2) known = if Set.member v1 known then v2 else v1

prim :: EdgeCache -> Set.Set Int -> Set.Set Int -> ConnectEdgeCache -> [ Edge ] -> [ Edge ]
prim edgeCache known unknown connectCache res = case Heap.uncons connectCache of
  Nothing -> res
  Just (entry@(Heap.Entry _w pair@(v1, v2)), rest) -> 
    if Set.member v1 known && Set.member v2 known then
      prim edgeCache known unknown rest res
    else do
      let unknownV = queryUnknown pair known
      let edgeCacheNext = removeKnown edgeCache known unknownV
      let knownNext = Set.insert unknownV known
      let connectCacheNext = getAdjs unknownV edgeCacheNext rest
      prim edgeCacheNext knownNext (Set.delete unknownV unknown) connectCacheNext (entryToTuple entry:res)
