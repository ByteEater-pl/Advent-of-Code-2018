import Data.List
 
main = do
  input <- getContents
  print (head [map fst (i++r) |
    h:t <- tails (lines input),
    s <- t,
    (i,_:r) <- [span eq (zip h s)],
    all eq r])
 
eq = uncurry (==)