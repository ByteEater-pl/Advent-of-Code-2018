import qualified Data.IntMap.Strict as M
import Control.Arrow
import Data.List
import Data.Ord
import Data.Array.IO

main = do
  input <- getContents
  let
    (_,_,tally,_) = foldl (\(i,m1,guards,awake) entry -> let
        asleep m =
          if awake then guards
          else M.adjust (((m1,m):)***(m-m1+)) i guards
        in case dropWhile (/= '#') entry of
          _:r -> let new = fst (head (reads r)) in
            (new, 0, M.insertWith (const id) new ([],0) (asleep 60), True)
          _ -> let [(m2,_)] = reads (drop 15 entry) in
            (i, m2, asleep m2, not awake))
      (0,0,M.empty,True)
      (sort (lines input))
    (id',(naps,_)) = maximumBy (comparing (snd.snd)) (M.toList tally)
  watch <- newArray (0,59) 0
  mapM (\(s,e) -> mapM (\j -> do
        n <- readArray (watch :: IOArray Int Integer) j
        writeArray watch j (n+1))
      [s..e-1])
    naps
  counts <- getElems watch
  print (id' * snd (maximum (zip counts [0..])))