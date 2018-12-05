{-# LANGUAGE PartialTypeSignatures #-}
import Data.IORef
import qualified Data.Set as S
import Data.Array.IO
import Data.List
import Data.Maybe
import Data.Char

main = do
  input <- getContents
  set <- newIORef S.empty
  fabric <- newArray ((0,1),(999,1000)) Nothing
  mapM (\claim -> do
    let
      id':left:top:width:height:_ =
        unfoldr (listToMaybe . reads . dropWhile (not . isDigit)) claim :: [Int]
    modifyIORef set (S.insert id')
    mapM (\i -> do
      x <- readArray (fabric :: IOArray _ _) i
      case x of
        Just n -> modifyIORef set (S.delete id' . S.delete n)
        Nothing -> writeArray fabric i (Just id'))
      (range ((left, top), (left + width - 1, top + height - 1))))
    (lines input)
  s <- readIORef set
  print (S.findMin s)