{-# LANGUAGE BangPatterns #-}

import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as BS

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

getIntBS :: BS.ByteString -> Int
getIntBS = fst . fromJust . BS.readInt

getIntListBS :: [ BS.ByteString ] -> [ Int ]
getIntListBS = map getIntBS

getIntTupleBS :: [ BS.ByteString ] -> (Int, Int)
getIntTupleBS = tuplify2 . map getIntBS

main :: IO ()
main = do
  [_n, _q] <- readIntList
  a <- readIntList
  pairs <- readIntTupleList
  mapM_ (print . findPos a 1 1) pairs
  where
    findPos :: [ Int ] -> Int -> Int -> (Int, Int) -> Int
    findPos [] _ _ _ = -1
    findPos (x:xs) !acc idx (xi, ki)
      | acc == ki && xi == x = idx
      | xi == x = findPos xs (acc + 1) (idx + 1) (xi, ki)
      | otherwise = findPos xs acc (idx + 1) (xi, ki)
    helper :: [ Int ] -> (Int, Int)
    helper [x1, x2] = (x1, x2)
    helper _ = undefined
