import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.List (unfoldr, concatMap)
import Data.Maybe (fromJust, fromMaybe)
import Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Strict as IM
import qualified Data.HashMap.Strict as HM

tuplify2 :: [Int] -> (Int, Int)
tuplify2 [x, y] = (x, y)
tuplify2 _ = undefined

bsReadIntList :: IO [ Int ]
bsReadIntList = bsGetIntList <$> BS.getLine

bsGetIntList :: BS.ByteString -> [ Int ]
bsGetIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

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
  [_n, q] <- bsReadIntList
  a <- bsReadIntList
  pairs <- readIntTupleListN q
  let pairsMap = makeIndices a
  mapM_ (print . findPos pairsMap) pairs
  where
    findPos :: IM.IntMap (IM.IntMap Int)  -> (Int, Int) -> Int
    findPos indices (xi, ki) = case IM.lookup xi indices of
      Just indices2 -> IM.findWithDefault (-1) ki indices2
      Nothing -> -1
    makeIndices :: [ Int ] -> IM.IntMap (IM.IntMap Int) 
    makeIndices a = IM.map (IM.fromList . rzip) (IM.fromListWith (<>) . map (second (pure :: Int -> [ Int ])) . zip a $ [ 1.. ])
    rzip :: [ Int ] -> [(Int, Int)]
    rzip xs = zip [l, pred l .. 1] xs
      where
        l = length xs
