import Data.List
import Data.Char
import Control.Monad 
import System.Random 
import System.IO
import Control.Monad (liftM)
import Foreign.C.Types
import System.Console.ANSI

main :: IO ()
main = do 
  seed <- newStdGen
  let initialBoard = addRandom [[0,0,0,0],[0,0,0,0],[0,0,0,0],[0,0,0,0]] seed
  seed2 <- newStdGen
  playGame (addRandom initialBoard seed2) 0

playGame board score = do
  clearScreen
  putStrLn $ "\nWelcome to the 2048 game in Haskell!\nUse WASD to make a move. Press c to quit, r to reset.\n\nScore: " ++ (show score) ++ "\n"
  showBoard board
  seed <- newStdGen
  input <- getHiddenChar
  case input of
    'c' -> return ()
    'r' -> main 
    input -> do 
      let newBoard = tryMove board input
      let newScore = calculateScore newBoard
      if (newBoard /= board ) then (playGame (addRandom newBoard seed) newScore) else (playGame board score)

calculateScore :: [[Int]] -> Int
calculateSingleTileScore :: Int -> Int
calculateSingleTileScore' :: Int -> Int -> Int
calculateScore board = foldr1 (+) $ map calculateSingleTileScore $ concat board
calculateSingleTileScore tile = calculateSingleTileScore' tile 1
calculateSingleTileScore' 0 _ = 0
calculateSingleTileScore' 2 _ = 0
calculateSingleTileScore' tile counter = tile*counter + (calculateSingleTileScore' (quot tile 2) (counter*2))

tryMove :: [[Int]] -> Char -> [[Int]]
tryMove board dir
  | dir == 'w' = transpose' $ map tryMerge $ transpose' board
  | dir == 'a' = map tryMerge board
  | dir == 's' = reverse $ transpose' $ map tryMerge $ map reverse $ transpose' board
  | dir == 'd'  =  map reverse $ map tryMerge $ map reverse board
  | otherwise = board

tryMerge :: [Int] -> [Int]
tryMerge xs = addZeroes $ tryMerge' (filter (/=0) xs)

tryMerge' :: [Int] -> [Int]
tryMerge' [] = []
tryMerge' [x] = [x]
tryMerge' (x:xs) 
   | x == head xs = x*2 : tryMerge' (tail xs)
   | otherwise = x : tryMerge' xs

addZeroes :: [Int] -> [Int]
addZeroes xs = xs ++ replicate (4 - length xs) 0 

transpose' :: [[Int]] -> [[Int]]
transpose' board 
  | length (head board) > 1 = map head board : transpose (map tail board)
  | length (head board) == 1 = [map head board]
  | otherwise = [[]]
  
addRandom :: [[Int]] -> StdGen -> [[Int]]
addRandom board seed = replaceNthZero board $ fst $ randomR (1, (length (filter (==0) (concat board)))) seed

replaceNthZero :: [[Int]] -> Int -> [[Int]]
replaceNthZero board n = unConcat (replaceNthZero' (concat board) n 0) 4

replaceNthZero' :: [Int] -> Int -> Int -> [Int]
replaceNthZero' [] _ _ = []
replaceNthZero' (x:xs) n counter
  | (n == (counter+1)) && (x == 0) = 2 : replaceNthZero' xs n (counter+1)
  | x == 0 =  x : replaceNthZero' xs n (counter+1)
  | otherwise = x : replaceNthZero' xs n (counter)
  
unConcat :: [Int] -> Int -> [[Int]]
unConcat [] _ = [[]]
unConcat xs n 
  | (mod (length xs)) n /= 0 = [[]]
  | length xs == n = [xs]
  | otherwise = [(take n xs)] ++ unConcat (drop n xs) n
  
showBoard :: (Show a, Foldable t) => t [a] -> IO ()
showBoardSpacing :: [[Char]] -> [Char]
showBoardAlign :: [Char] -> [Char]
showBoard board = putStrLn $ showBoardAlign $ showBoardSpacing $ map show (concat board)
showBoardSpacing [] = []
showBoardSpacing (x:xs) 
 | length x > 6 = x ++ showBoardSpacing xs 
 | otherwise = take (5 - (length x)) (repeat ' ') ++ x ++ showBoardSpacing xs 
showBoardAlign [] = []
showBoardAlign board = take 20 board ++ "\n" ++ showBoardAlign (drop 20 board) -- 20 because showBoardSpacing adds up to 5 spaces

getHiddenChar = liftM (chr.fromEnum) c_getch  -- workaround for "hSetBuffering stdin NoBuffering" not working on Windows
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt