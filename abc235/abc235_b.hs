main :: IO ()
main = do
  nStr <- getLine
  let _n = read nStr :: Int
  hiStr <- getLine
  let (x:xs) = map read $ words hiStr :: [ Int ]
  print . search xs $ x
  where
    search :: [ Int ] -> Int -> Int
    search [] prev = prev
    search (x:xs) prev = if x <= prev then prev else search xs x
