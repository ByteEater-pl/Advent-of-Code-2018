main = do
  input <- getContents
  print (sum [read (if h == '+' then t else x) | x@(h:t) <- lines input])