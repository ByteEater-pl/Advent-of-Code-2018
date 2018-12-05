{-# LANGUAGE PartialTypeSignatures, LambdaCase #-}
import Data.Array.IO
import Data.List
import Data.Maybe
import Data.Array

data Measure = Zero | One | More

main = do
  input <- getContents
  fabric <- newArray ((0,1),(999,1000)) Zero
  mapM (\claim -> do
    let
      left:top:width:height:_ =
        unfoldr (listToMaybe . reads . tail) (dropWhile (/= '@') claim) :: [Int]
    mapM (\i -> do
      x <- readArray fabric i
      writeArray fabric i $ case x of
        Zero -> One
        _ -> More)
      (range ((left, top), (left + width - 1, top + height - 1))))
    (lines input)
  frozen <- freeze (fabric :: IOArray _ _)
  print (sum ((\case More -> 1; _ -> 0) <$> frozen :: Array _ Int))