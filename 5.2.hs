import qualified Data.Sequence as S
import Data.List

main = do
  input <- getContents
  let nums = map fromEnum input
  print (minimum [react (S.filter (`notElem` [c, c+32]) (S.fromList nums)) 0
    | c <- nub nums, c < 91])

react p i =
  let
    l = S.length p
    j = i+1
  in if l<=j then l
    else if abs (S.index p i - S.index p j) == 32 then
      react (S.deleteAt i (S.deleteAt i p)) (max (i-1) 0)
    else react p j