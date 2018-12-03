import Data.List
 
main = do
  input <- getContents
  let count n = length (filter (elem n) (map (map length . group . sort) (lines input)))
  print (count 2 * count 3)