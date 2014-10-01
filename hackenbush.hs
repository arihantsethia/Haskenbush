module HackenBush where
import Data.Char
import Data.List
import Data.Maybe
import Prelude
import System.IO

type Board = [Bush]
type Heap = Integer
type Turn = (Integer, Integer)

players = []

main = do putStrLn " Enter 1 for Single Player and 2 for multiplayer."
          option <- readLn
          gameBoard = GenerateRandomBoard 0
          --gameBoard = []
          --DisplayGame gameBoard 0
          if option == 1 then
              putStrLn "Name of Player 1 :"
              player1 <- readLn
              putStrLn "Name of Player 2 :"
              player2 <- readLn
              players ++ player1 ++ player2
              HackenBushAI gameBoard
          else
              putStrLn "Name of Player 1 :"
              player1 <- readLn
              player2 = "Computer"
              players ++ player1 ++ player2
              HackenBush gameBoard

GenerateRandomBoard :: Integer -> Board
GenerateRandomBoard 0 = []

HackenBush :: Board -> IO Board
HackenBush [] = return []
HackenBush xs = do
                  board <- play 1 xs
                  DisplayGame board 1
                  newBoard <- play 2 board
                  DisplayGame newBoard 2
                  HackenBush newBoard

play :: Integer -> Integer -> Board -> IO Board
play player xs = do 
                   putStrLn " "
                   putStrLn "Player" ++ player ++ ": Enter The Row From Which You Would Like To Take"
                   bushNumber <- readLn
                   putStrLn " "
                   putStrLn "Player" ++ player ++ ":Enter The Amount You Would Like To Take"
                   branch <- readLn
                   putStrLn " "
                   putStrLn "HackenBush Game Status: "
                   putStrLn " "
                   --return $ update_game xs bushNumber branch

DisplayGame :: Board -> Int -> IO ()
DisplayGame [] _ = return()

--update the game state --
--Current game state -> user options (row and how many to take) -> new state --
update_game :: [Int] -> Int -> Int -> [Int]
update_game [] _ _ = []
update_game [0,0,0] _ _ = []
update_game (x:xs) row take_amnt | sum(x:xs) == 0 = []
                                | row == 1 = x - take_amnt:xs
                                 | row == 2 = x : head(xs) - take_amnt : tail(xs)
                                 | row == 3 = x : head(xs) : [last(xs) - take_amnt]