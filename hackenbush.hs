module HackenBush where
import Data.Char
import Data.List
import Data.Maybe
import Prelude
import System.IO
import System.Random

type Turn = (Integer, Integer)
type Board = [Bush BranchObject]
data Color = Red | Green | Blue deriving (Read, Show, Enum, Eq, Ord)
data Bush a = Empty | Leaf a | Branch a [Bush a] deriving Show

players :: [String]
players = []

main :: IO ()
main = do 
    inputOption <- promptLine "Enter 1 for Single Player and 2 for multiplayer."
    option <- getInteger inputOption
    players <- getInput option
    numberOfBushes <- randomRIO (1, 15)
    let board = generateRandomBoard1 0
    let x = startGame board option
    print 5

getInput :: (Eq a, Num a) => a -> IO [String]
getInput option
    | option == 1 = do
          player1 <- promptLine "Name of Player 1 :"
          let player2 = "Jarvis"
          let playerNames = [player1, player2]
          return playerNames
    | option == 2 = do 
          player1 <- promptLine "Name of Player 1 :"
          player2 <- promptLine "Name of Player 2 :"
          let playerNames = [player1, player2]
          return playerNames
    | otherwise = error "Not a valid option"

startGame :: (Eq a, Num a) => Board -> a -> IO Board
startGame board option
    | option == 1 =  hackenBush board 1
    | option == 2 =  hackenBush board 1
    | otherwise = error "Not a valid option"

hackenBush :: Board -> Int -> IO Board
hackenBush [] _= return []
hackenBush board player = do
    newBoard <- play board player
    hackenBush newBoard (mod player 2 + 1)

hackenBushAI :: Board -> Int -> IO Board
hackenBushAI [] _= return []
hackenBushAI board player
    | player == 1 = do
          newBoard <- play board player
          hackenBushAI newBoard 2
    | player == 2 = do 
          newBoard <- playAI board
          hackenBushAI newBoard 1

play :: Board -> Int -> IO Board
play board player  = do
    let playerName = players!!player
    bushNumber <- promptLine $ playerName ++ " : Enter The Row From Which You Would Like To Take :"
    print bushNumber
    branch <- promptLine $ playerName ++ " : Enter The Branch You Would Like To Take :"
    print branch
    return board

playAI board = do
    return board

--generateRandomNumber :: Integer -> Integer -> Integer
generateRandomNumber st en = do
    randomNumberIO <- randomRIO (st, en)
    --randomNumber <- getInteger randomNumberIO
    return randomNumberIO

generateRandomBush g 0 = [Empty]
generateRandomBush g h = do
    let numberOfSubBushes = generateRandomNumber 0 5
    if numberOfSubBushes == 0 then return [Empty]
    else return [(generateRandomBush x (h-1))| x <- [1..numberOfSubBushes]] 

generateRandomBoard1 0 = [Empty]
--generateRandomBoard :: Integer -> Board
generateRandomBoard g 0 = []
generateRandomBoard g d = do
    let height = generateRandomNumber 1 6
    return [generateRandomBush g height| x <- [1..d]] 

getInteger string = do
    let integer = read string :: Integer
    return integer

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    getLine