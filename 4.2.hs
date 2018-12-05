{-# LANGUAGE TupleSections #-}
import Data.List
import qualified Data.IntMap.Strict as M
import Data.Array.IO
import Data.Ord
 
main = do
  input <- getContents
  let
    (_,_,tally,_) = foldl (\(i,m1,guards,awake) entry -> let
        asleep m =
          if awake then guards
          else M.adjust ((m1,m):) i guards
        in case dropWhile (/= '#') entry of
          _:r -> let new = fst (head (reads r)) in
            (new, 0, M.insertWith (const id) new [] (asleep 60), True)
          _ -> let [(m2,_)] = reads (drop 15 entry) in
            (i, m2, asleep m2, not awake))
      (0,0,M.empty,True)
      (sort (lines input))
  totals <- mapM (\(id',naps) -> do
      watch <- newArray (0,59) 0
      mapM (\(s,e) -> do
          mapM (\j -> do
              n <- readArray (watch :: IOArray Int Integer) j
              writeArray watch j (n+1))
            [s..e-1])
        naps
      map (id',) <$> getAssocs watch)
    (M.toList tally)
  let (g,(t,_)) = maximumBy (comparing (snd.snd)) (concat totals)
  print (g*t)