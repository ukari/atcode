import Control.Monad (replicateM)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IM

tuplify2 :: [Int] -> (Int, Int)
tuplify2 [x, y] = (x, y)
tuplify2 _ = undefined

readIntList :: IO [ Int ]
readIntList = do
  bts <- BS.getLine
  pure . getIntListBS . BS.words $ bts

readIntTuple :: IO (Int, Int)
readIntTuple = do
  bts <- BS.getLine
  pure . getIntTupleBS . BS.words $ bts

readIntTupleList :: IO [ (Int, Int) ]
readIntTupleList = map (getIntTupleBS . BS.words) . BS.lines <$> BS.getContents

readIntTupleListN :: Int -> IO [ (Int, Int) ]
readIntTupleListN = flip replicateM (getIntTupleBS . BS.words <$> BS.getLine)

getIntBS :: BS.ByteString -> Int
getIntBS = fst . fromJust . BS.readInt

getIntListBS :: [ BS.ByteString ] -> [ Int ]
getIntListBS = map getIntBS

getIntTupleBS :: [ BS.ByteString ] -> (Int, Int)
getIntTupleBS = tuplify2 . map getIntBS

main :: IO ()
main = do
  [_n, q] <- readIntList
  a <- readIntList
  pairs <- readIntTupleListN q
  let pairsMap = makeIndices a
  print pairsMap
  print pairs
  mapM_ (print . findPos pairsMap) pairs
  where
    findPos :: IM.IntMap (IM.IntMap Int) -> (Int, Int) -> Int
    findPos indices (xi, ki) = case IM.lookup xi indices of
      Just indices2 -> fromMaybe (-1) (IM.lookup ki indices2)
      Nothing -> -1
    makeIndices :: [ Int ] -> IM.IntMap (IM.IntMap Int)
    makeIndices = foldr (\(idx, cur) acc ->
      IM.insertWith IM.union cur (makeInsert acc cur idx) acc
      ) IM.empty . reverse . zip [ 1.. ]
      where
        makeInsert :: IM.IntMap (IM.IntMap Int) -> Int -> Int -> IM.IntMap Int
        makeInsert ids k i = case IM.lookup k ids of
          Just old -> IM.singleton (IM.size old + 1) i
          Nothing -> IM.singleton 1 i
