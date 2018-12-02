import Data.Set

main = do
  input <- getContents
  print (search 0 (cycle [read (if h == '+' then t else x) | x@(h:t) <- lines input]) empty)

search acc (c:cs) set =
  if member acc set then acc
  else search (acc + c) cs (insert acc set)