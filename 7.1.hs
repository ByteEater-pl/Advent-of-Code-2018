import qualified Data.Set as S
import Data.List

main = do
  input <- getContents
  let [preds,succs] = map (`map` lines input) [(!!5),(!!36)] in
    print (topSort
      (zip preds succs)
      (S.fromList (nub (preds++succs) \\ succs))
      [])

topSort dag set rev =
  if null set then reverse rev else
    let
      (step,subset) = S.deleteFindMin set
      (outs,rest) = partition ((==step).fst) dag
    in topSort
      rest
      (S.union (S.fromList [v | (_,v) <- outs, all ((/=v).snd) rest]) subset)
      (step:rev)