import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.List (unfoldr)
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
    findPos indices (xi, ki) = fromMaybe (-1) (HM.lookup (xi, ki) indices)
    makeIndices :: [ Int ] -> HM.HashMap (Int, Int) Int
    makeIndices = fst . foldr (\(idx, cur) (acc1, acc2) ->
      (HM.insert (cur, lookup2 acc2 cur) idx acc1 , makeInsert2 acc2 cur)) (HM.empty, IM.empty :: IM.IntMap Int) . reverse . zip [ 1.. ]
      where
        lookup2 :: IM.IntMap Int -> Int -> Int
        lookup2 ids k = case IM.lookup k ids of
          Just old -> old + 1
          Nothing -> 1
        makeInsert2 :: IM.IntMap Int -> Int -> IM.IntMap Int
        makeInsert2 ids k = IM.insert k (lookup2 ids k) ids
