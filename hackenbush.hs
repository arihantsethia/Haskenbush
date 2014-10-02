module HackenBush where
import Data.Char
import Data.List
import Data.Maybe
import Prelude
import System.IO
import System.Random

type BranchObject = (Integer, Color)
type Board = [Bush Color]
data Color = Red | Green | Blue deriving (Read, Show, Enum, Eq, Ord)
data Bush a = Empty | Leaf a | Branch a [Bush a] deriving (Read, Show, Eq, Ord)

players :: [String]
players = []

main :: IO ()
main = do 
    inputOption <- promptLine "Enter 1 for Single Player and 2 for multiplayer : "
    option <- getInteger inputOption
    inputDiff <- promptLine "Enter 1 for Easy, 2 For Medium and 3 for Difficult : "
    difficulty <- getInteger inputDiff
    players <- getInput option
    g <- newStdGen
    let randTuple = randomNumber g 2 (3*difficulty)
    let numberOfBushes = fst randTuple
    let g' = snd randTuple
    let board = randomBoard g' numberOfBushes difficulty
    print board
    let x = startGame board option
    print "Play Again!"

getInput :: (Eq a, Num a) => a -> IO [String]
getInput option
    | option == 1 = do
          player1 <- promptLine "Name of Player 1 :"
          let player2 = "Jarvis"
          return [player1, player2]
    | option == 2 = do 
          player1 <- promptLine "Name of Player 1 : "
          player2 <- promptLine "Name of Player 2 : "
          return [player1, player2]
    | otherwise = error "Not a valid option"

startGame :: (Eq a, Num a) => Board -> a -> IO Board
startGame board option
    | option == 1 =  hackenBush board 1
    | option == 2 =  hackenBush board 1
    | otherwise = error "Not a valid option"

hackenBush [] _= return []
hackenBush board player = hackenBush newBoard (mod player 2 + 1) where newBoard = play board player

play board player = 
    let playerName = players!!player
        bushNumber = promptLine $ playerName ++ " : Enter The Row From Which You Would Like To Take :"
        branch = promptLine $ playerName ++ " : Enter The Branch You Would Like To Take :"
    in board

playAI board = board

randomColor :: RandomGen g => g -> (Color, g)
randomColor g = case randomR (0,2) g of (r, g') -> (toEnum r, g')

randomNumber :: RandomGen g => g -> Integer -> Integer -> (Integer, g)
randomNumber g st en = do
    randomR (st::Integer, en::Integer) g

randomBushTuple :: (Num a, Ord a, RandomGen g) => g -> a -> Integer -> (Bush Color, g)
randomBushTuple g h difficulty
    | h <= 0 = (Empty, g)
    | otherwise = let randColorTuple = randomColor g
                      color = fst randColorTuple
                      g' = snd randColorTuple
                      randTuple = randomNumber g' 0 (2*difficulty)
                      numberOfSubBushes = fst randTuple
                      g'' = snd randTuple
                      bush = Branch color $ randomBushList g'' (h-1) numberOfSubBushes difficulty
                  in (bush, g'')    

randomBushList :: (Num a, Ord a, RandomGen g) => g -> a -> Integer -> Integer -> [Bush Color]
randomBushList _ _ 0 _= []
randomBushList g h numberOfSubBushes difficulty= 
    let bushTuple = randomBushTuple g (h-1) difficulty
        bush = fst bushTuple
        g' = snd bushTuple
    in deleteAllInstances Empty (bush : randomBushList g' h (numberOfSubBushes-1) difficulty)

randomBoard :: (Num a, Ord a, RandomGen b) => b -> a -> Integer -> [Bush Color]
randomBoard g d difficulty
    | d<=0 = []
    | otherwise = let randTuple = randomNumber g 1 (2*difficulty + 1)
                      height = fst randTuple
                      g' = snd randTuple
                      bushTuple = randomBushTuple g' height difficulty
                      bush = fst bushTuple
                      g'' = snd bushTuple
                  in bush : randomBoard g'' (d-1) difficulty

deleteAllInstances :: Eq a => a -> [a] -> [a]
deleteAllInstances a (x:xs)
    | a == x    = rest
    | otherwise = x : rest
      where
        rest = deleteAllInstances a xs
deleteAllInstances _ _ = []

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs

getInteger :: String -> IO Integer
getInteger string = do
    let integer = read string :: Integer
    return integer

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    getLine