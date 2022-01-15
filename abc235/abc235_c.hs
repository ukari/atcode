main :: IO ()
main = do
  nqStr <- getLine
  let [_n, q] = map read (words nqStr) :: [Int]
  aStr <- getLine
  let a = map read (words aStr) :: [ Int ]
  pairs <- load q []
  mapM_ (print . findPos a 1 1) pairs 
  where
    load :: Int -> [(Int, Int)] -> IO [(Int, Int)]
    load 0 acc = pure $ reverse acc
    load n acc = do
      pairsStr <- getLine
      let pair = helper (map read (words pairsStr)) :: (Int, Int)
      print pair
      load (n - 1) (pair : acc)
    findPos :: [Int] -> Int -> Int -> (Int, Int) -> Int
    findPos [] _ _ _ = -1
    findPos (x:xs) acc idx (xi, ki)
      | acc == ki && xi == x = idx
      | xi == x = findPos xs (acc + 1) (idx + 1) (xi, ki)
      | otherwise = findPos xs acc (idx + 1) (xi, ki)
    helper :: [ Int ] -> (Int, Int)
    helper [x1, x2] = (x1, x2)
