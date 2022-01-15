main :: IO ()
main = do
  inputStr <- getLine
  let input = read inputStr :: Int
  let result = sum . map (sumn . extract input) $ [0 .. 2]
  print result
  where
    extract :: Int -> Int -> Int
    extract input n = floor (fromIntegral (input `mod` (10 ^ (n + 1))) / (10 ^ n) :: Double)
    sumn :: Int -> Int
    sumn n = n + 10 * n + 100 * n
