import Data.List
import Data.Char

testBoard = [[2,2,0,0],[4,2,0,0],[4,2,0,0],[0,2,0,0]]

tryMove :: [[Integer]] -> Char -> [[Integer]]
tryMove board dir
  | dir == 'w' = [mapCols tryMerge (board!!0) (board!!1),  mapCols tryMerge (board!!1) (board!!2), mapCols tryMerge (board!!2) (board!!3), board!!3]
  | otherwise = []

tryMerge :: Integer -> Integer -> Integer
tryMerge a b 
  | a == 0 = b
  | a == b = a*2
  | otherwise = a
  
mapCols :: (a -> b -> c) -> [a] -> [b] -> [c]
mapCols _ [] [] = []
mapCols f (x:xs) (y:ys) = f x y : mapCols f xs ys

showBoard board = mapM_ putStrLn $ map show $ board

