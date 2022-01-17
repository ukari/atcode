import Data.List (unfoldr)
import Data.Char (isSpace)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as Set

bsReadIntList :: IO [ Int ]
bsReadIntList = bsGetIntList <$> BS.getLine

bsGetIntList :: BS.ByteString -> [ Int ]
bsGetIntList = unfoldr (BS.readInt . BS.dropWhile isSpace)

main :: IO ()
main = do
  [ a, n ] <- bsReadIntList
  print $ isearch [ (n, 0) ] a Set.empty

im1 :: Int -> Int -> Int
im1 x a = if x `mod` a == 0
  then x `div` a
  else -1

im2 :: Int -> Int
im2 num = if res /= num && l res == l num then res else -1
  where
    extractH :: Int -> Int
    extractH x = x `div` 10 ^ l x
    extractL :: Int -> Int
    extractL x = x * 10 - extractH x * 10 ^ (l x + 1)
    l :: Int -> Int
    l = floor . logBase (10 :: Float) . fromIntegral
    res :: Int
    res = extractL num + extractH num

isearch :: [ (Int, Int) ] -> Int -> Set.Set Int -> Int
isearch pairs a cache = if null results
  then if null next
       then -1
       else isearch next a (updateCache pairs cache)
  else snd . head $ results
  where
    next :: [ (Int, Int) ]
    next = filter (not . flip Set.member cache . fst) . isearch2 pairs $ a
    results :: [ (Int, Int) ]
    results = filter ((==1) . fst) pairs
    updateCache :: [ (Int, Int) ] -> Set.Set Int -> Set.Set Int
    updateCache ps ca = Set.union ca (Set.fromList $ fst <$> ps)

isearch2 :: [ (Int, Int) ] -> Int -> [ (Int, Int) ]
isearch2 pairs a = Set.toList . Set.fromList . foldr (\cur acc ->
    sm cur a <> acc
  ) [] $ pairs

sm :: (Int, Int) -> Int -> [ (Int, Int) ]
sm (x, step) a
  | x < 1 = []
  | x == 1 = [(x, step)]
  | x < 10 = [(im1 x a, step + 1)]
  | otherwise = [(im1 x a, step + 1), (im2 x, step + 1)]
