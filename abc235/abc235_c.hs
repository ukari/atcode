import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.List (unfoldr, concatMap)
import Data.Maybe (fromJust, fromMaybe)
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
    findPos :: HM.HashMap (Int, Int) Int -> (Int, Int) -> Int
    findPos indices (xi, ki) = HM.lookupDefault (-1) (xi, ki) indices
    makeIndices :: [ Int ] -> HM.HashMap (Int, Int) Int
    makeIndices = HM.fromList . fst . foldr (\(idx, cur) (acc1, acc2) ->
      ( ((cur, IM.findWithDefault 0 cur acc2 + 1), idx) : acc1
      , IM.insert cur (IM.findWithDefault 0 cur acc2 + 1) acc2))
      ([], IM.empty :: IM.IntMap Int) . reverse . zip [ 1.. ]
