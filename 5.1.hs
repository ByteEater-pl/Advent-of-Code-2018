import qualified Data.Sequence as S

main = do
  input <- getContents
  print (react (S.fromList (map fromEnum input)) 0)
 
react p i =
  let
    l = S.length p
    j = i+1
  in if l<=j then l
    else if abs (S.index p i - S.index p j) == 32 then
      react (S.deleteAt i (S.deleteAt i p)) (max (i-1) 0)
    else react p j