  
  
  
  
  
  
import Data.List
import Data.Char
import Control.Monad 
import System.Random 
import System.Console.Haskeline
import Data.Maybe
import System.IO

testBoard = [[2,2,0,2],[4,2,0,2],[4,2,0,0],[0,2,4,2]]

main = do
  hSetBuffering stdin NoBuffering
  ioLoop board = do
    seed <- newStdGen
    runInputT defaultSettings (loop [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]] seed)
      where 
      --  loop :: InputT IO ()
        loop board seed = do
            minput <- getInputChar "% "
            case minput of
                Nothing -> return ()
                Just 'q' -> return ()
                Just input -> do outputStrLn $ showBoard $ tryMove board (fromJust minput)
                                 (ioLoop (addRandom (tryMove board (fromJust minput)) (fst (randomR (0, (length (filter (==0) (concat board)))) seed))))
  
tryMove :: [[Integer]] -> Char -> [[Integer]]
tryMove board dir
  | dir == 'w' = transpose' $ map tryMerge $ transpose' board
  | dir == 'a' = map tryMerge board
  | dir == 's' = reverse $ transpose' $ map tryMerge $ map reverse $ transpose' board
  | dir == 'd' =  map reverse $ map tryMerge $ map reverse board
  | otherwise = [[]]
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

showBoard :: [[Integer]] -> String
showBoard [] = []
showBoard board = show (take 4 (head board)) ++ "\n" ++ (showBoard (tail board))
--showBoard board = show $ map (++ "\newline") $ map show $ board

transpose' :: [[Integer]] -> [[Integer]]
transpose' board 
  | length (head board) > 1 = map head board : transpose (map tail board)
  | length (head board) == 1 = [map head board]
  | otherwise = [[]]
addRandom :: [[Integer]] -> Int -> [[Integer]]
addRandom board n = unConcat (replaceNthZero (concat board) n) 4 --unConcat (replaceNthZero (concat board) (fst (randomR (0, (length (filter (==0) (concat board)))) seed))) 4
unConcat :: [Integer] -> Int -> [[Integer]]
unConcat [] _ = [[]]
unConcat xs n 
  | (mod (length xs)) n /= 0 = [[]]
  | length xs == n = [xs]
  | otherwise = [(take n xs)] ++ unConcat (drop n xs) n
replaceNthZero :: [Integer] -> Int -> [Integer]
replaceNthZero list n = replaceNthZero' list n 0
replaceNthZero' :: [Integer] -> Int -> Int -> [Integer]
replaceNthZero' [] _ _ = []
replaceNthZero' (x:xs) n counter
  | (n == (counter+1)) && (x == 0) = 2 : replaceNthZero' xs n (counter+1)
  | x == 0 =  x : replaceNthZero' xs n (counter+1)
  | otherwise = x : replaceNthZero' xs n (counter)