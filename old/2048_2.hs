import Data.List
import Data.Char
import Control.Monad  

testBoard = [2,2,0,2,4,2,0,2,4,2,0,0,0,2,4,2]
testBoardA = [4,2,0,0,4,4,0,0,4,2,0,0,2,4,2,0]
testBoardD = [0,0,2,4,0,0,4,4,0,0,4,2,0,2,4,2]
testBoardS = [0,0,0,0,0,0,0,0,2,4,0,2,8,4,4,4]

main :: IO ()
main = playGame initialCheckersBoard 'a'

initialCheckersBoard = [2,2,0,2,4,2,0,2,4,2,0,0,0,2,4,2]

playGame board dir = do
  printBoard board dir
  l <- getLine
  playGame (tryMove board (head l)) (head l)

printBoard :: [Integer] -> Char -> IO ()
printBoard board dir
  | dir == 'w' = showBoard2 board
  | dir == 'a' = showBoard board
  | dir == 's' = showBoard3 board
  | dir == 'd' = showBoard board
  | otherwise = showBoard [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]

tryMove :: [Integer] -> Char -> [Integer]
tryMove board dir
  | dir == 'w' = tryMerge (takeEachFourth board 0) ++ tryMerge (takeEachFourth board 1) ++ tryMerge (takeEachFourth board 2) ++ tryMerge (takeEachFourth board 3)
  | dir == 'a' = tryMerge (sublist 0 4 board) ++ tryMerge (sublist 4 8 board) ++ tryMerge (sublist 8 12 board) ++ tryMerge (sublist 12 16 board)
  | dir == 's' = tryMerge (reverse (takeEachFourth board 0)) ++ tryMerge (reverse (takeEachFourth board 1)) ++ tryMerge (reverse (takeEachFourth board 2)) ++ tryMerge (reverse (takeEachFourth board 3))
  | dir == 'd' = reverse (tryMerge (sublist 12 16 board) ++ tryMerge (sublist 8 12 board) ++ tryMerge (sublist 4 8 board) ++ tryMerge (sublist 0 4 board))
  | otherwise = []
  
sublist start end =  drop start . take end
  
tryMerge :: [Integer] -> [Integer]
tryMerge xs = addZeroes $ tryMerge' (filter (/=0) xs)

addZeroes :: [Integer] -> [Integer]
addZeroes xs = xs ++ replicate (4 - length xs) 0 

tryMerge' :: [Integer] -> [Integer]
tryMerge' [] = []
tryMerge' [x] = [x]
tryMerge' (x:xs) 
   | x == head xs = x*2 : tryMerge' (tail xs)
   | otherwise = x : tryMerge' xs
  
mapCols :: (a -> b -> c) -> [a] -> [b] -> [c]
mapCols _ [] [] = []
mapCols f (x:xs) (y:ys) = f x y : mapCols f xs ys

showBoard board = mapM_ putStrLn $ map show $ showBoard' board
showBoard2 board = mapM_ putStrLn $ map show $ showBoard'' board
showBoard3 board = mapM_ putStrLn $ map show $ showBoard''' board
showBoard' [] = []
showBoard' board = take 4 board : showBoard' (drop 4 board)

showBoard'' board = [takeEachFourth board 0, takeEachFourth board 1, takeEachFourth board 2, takeEachFourth board 3 ]
showBoard''' board = [takeEachFourth board 3, takeEachFourth board 2, takeEachFourth board 1, takeEachFourth board 0 ]

takeEveryM m lst = [n | (i,n) <- zip [1..] lst, i `mod` m == 0]

takeEachFourth :: [Integer] -> Int -> [Integer]
takeEachFourth [] _ = []
takeEachFourth [x] _ = [x]
takeEachFourth list start 
  | start > 0 = takeEachFourth (tail list) (start-1)
  | otherwise = head list : takeEachFourth (drop 4 list) start
