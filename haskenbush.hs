import Data.Char
import Data.List
import Data.Maybe
import Graphics.UI.Gtk hiding (Cross)
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo hiding(fill)
import Graphics.UI.Gtk.Gdk.GC
import Graphics.UI.Gtk.Gdk.Events
import Prelude
import System.IO
import System.Random
import Data.IORef
import Data.HashMap as M

type HM = M.Map (Integer,Integer) Integer
type BranchObject = (Integer, BranchColor)
type Board = [Bush BranchObject]
data BranchColor = Red | Green | Blue | White deriving (Read, Show, Enum, Eq, Ord)
data Bush a = Empty | Leaf a | Branch a [Bush a] deriving (Read, Show, Eq, Ord)

players :: [String]
players = []

branchLength = -50

data GameState = GameState { board :: Board, playerName :: [String] , activePlayer :: Int, mp :: HM }
data GUI = GUI {
    getPlayBoard :: IO DrawingArea,
    drawWindow :: IO DrawWindow,
    disableBoard :: IO (),
    resetBoard :: IO (),
    setStatus :: String -> IO ()
}

data RGB = RGB {
    r :: Double,
    g :: Double,
    b :: Double
}

main :: IO ()
main = do
    initGUI

    Just xml <- xmlNew "haskenbush.glade"
    window <- xmlGetWidget xml castToWindow "mWindow"
    window `onDestroy` mainQuit

    newgame <- xmlGetWidget xml castToMenuItem "newGame"
    quit <- xmlGetWidget xml castToMenuItem "quit"
    help <- xmlGetWidget xml castToMenuItem "help"

    gameBoard <- xmlGetWidget xml castToDrawingArea "gameBoard"
    statusBar <- xmlGetWidget xml castToStatusbar "gameStatus"
    buttons <- mapM (xmlGetWidget xml castToButton) ["bPlayOk", "bDiffOk"]
    dialogs <- mapM (xmlGetWidget xml castToDialog) ["playDialog", "difficultyDialog"]
    cBox <- xmlGetWidget xml castToComboBox "cbDifficulty"
    ctx <- statusbarGetContextId statusBar "state"

    gui <- guiActions buttons dialogs cBox gameBoard statusBar ctx

    widgetModifyBg gameBoard StateNormal (Color 65535 65535 65535)

    widgetShowAll window

    let playBoard = [Empty]
    let playerNames = ["Player 1","Player 2"]
    let playMap = M.empty :: HM
    newGameState <- reset gui GameState { board = playBoard, playerName = playerNames, activePlayer = 0, mp = playMap }
    state <- newIORef newGameState
    let modifyState f = readIORef state >>= f >>= writeIORef state

    onActivateLeaf quit mainQuit
    onActivateLeaf newgame $ modifyState $ reset gui

    onButtonPress gameBoard (\e -> do
                                    let x = truncate (eventX e)
                                    let y = truncate (eventY e)
                                    modifyState $ removeBranch gui gameBoard x y
                                    --drawingBoard <- drawWindow gui
                                    --drawWindowClearAreaExpose drawingBoard 0 0 1000 1000
                                    return True
                                    )

    onExpose gameBoard (\x -> do
                                modifyState $ drawCanvas gameBoard
                                return True
                    )

    mainGUI
    {-inputOption <- promptLine "Enter 1 for Single Player and 2 for multiplayer : "
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
    print "Play Again!"-}

guiActions buttons dialogs cBox gameBoard statusBar ctx =
    do return $ GUI {disableBoard = flip widgetSetSensitivity False gameBoard,
                     resetBoard = do
                        drawWin <- widgetGetDrawWindow gameBoard
                        drawWindowClear drawWin
                        flip widgetSetSensitivity True gameBoard,
                     getPlayBoard = do return gameBoard,
                     drawWindow = do
                        drawWin <- widgetGetDrawWindow gameBoard
                        return drawWin,
                     setStatus = \msg -> do
                                 statusbarPop statusBar ctx
                                 statusbarPush statusBar ctx msg
                                 return ()}

reset gui (GameState _board _playerName _activePlayer _mp) = do
    resetBoard gui
    let difficulty = 2
    let players = 2
    g <- newStdGen
    let randTuple = randomNumber g 2 (3*difficulty)
    let numberOfBushes = fst randTuple
    let g' = snd randTuple
    let board = randomBoard g' numberOfBushes difficulty 0
    drawingBoard <- drawWindow gui
    let s = " : make your move." :: String
    setStatus gui $ _playerName!!_activePlayer ++ s
    drawWindowClearAreaExpose drawingBoard 0 0 1000 1000
    return (GameState board _playerName _activePlayer _mp)

drawCanvas gameBoard st@(GameState board playerName activePlayer mp) = do
    let numberOfBushes = fromIntegral $ (length board)
    let divLength =  900 / numberOfBushes
    let startPointX = 50 + divLength/2
    let startPointY = 550
    mp'<-drawBoard board mp gameBoard startPointX startPointY divLength
    return (GameState board playerName activePlayer mp')

removeBranch gui gameBoard x y st@(GameState board playerName activePlayer mp)= do
    let id = getId mp x y
    if id /= -1 
        then do let color = findInBushList board id
                if (isValidColor color activePlayer)
                    then do let newBoard = cutBush board id
                            let nextPlayer = mod ( activePlayer + 1 ) 2
                            drawingBoard <- drawWindow gui
                            drawWindowClearAreaExpose drawingBoard 0 0 1000 1000
                            if wins newBoard $ getColor nextPlayer 
                                then do let s = " : Wins." :: String
                                        setStatus gui $ playerName!!(mod (nextPlayer+1) 2) ++ s
                                        disableBoard gui
                                else do let s = " : make your move." :: String
                                        setStatus gui $ playerName!!nextPlayer ++ s
                            return (GameState newBoard playerName nextPlayer mp)
                    else do 
                        return st
        else do
            return st

wins board color 
    | findColorInBushList board color = False
    | findColorInBushList board Green = False
    | otherwise = True
getId mp x y
    |  member (x,y) mp = fromJust $ M.lookup (x,y) mp
    |  member (x,y+1) mp = fromJust $ M.lookup (x,y+1) mp
    |  member (x,y-1) mp = fromJust $ M.lookup (x,y-1) mp
    |  member (x+1,y) mp = fromJust $ M.lookup (x+1,y) mp
    |  member (x+1,y+1) mp = fromJust $ M.lookup (x+1,y+1) mp
    |  member (x+1,y-1) mp = fromJust $  M.lookup (x+1,y-1) mp
    |  member (x-1,y) mp = fromJust $ M.lookup (x-1,y) mp
    |  member (x-1,y+1) mp = fromJust $ M.lookup (x-1,y+1) mp
    |  member (x-1,y-1) mp = fromJust $ M.lookup (x-1,y-1) mp
    | otherwise = -1

getColor id
    | id == 0 = Red
    | id == 1 = Blue
    | otherwise = White

isValidColor color id
    | id == 0 && color == Red = True
    | id == 1 && color == Blue = True
    | color == Green = True
    | otherwise = False

drawBoard [] mp _ _ _ _= do return mp 
drawBoard (bush:board) mp canvas startPointX startPointY divLength= do
    mp' <- drawBush bush mp canvas startPointX startPointY 90
    mp'' <- drawBoard board mp' canvas (startPointX + divLength) startPointY divLength
    return mp''

drawBush (Branch branch bushes) mp canvas startPointX startPointY angle = do
    let endPoints = getEndPoints startPointX startPointY angle branchLength
    let endPointX = fst endPoints
    let endPointY = snd endPoints
    mp' <- drawBranch branch mp canvas startPointX startPointY endPointX endPointY
    let numberOfSubBushes = fromIntegral $ length bushes
    if numberOfSubBushes > 0 
        then do let angleDiff = 180.0 / numberOfSubBushes
                let startAngle = angle - 90.0 + angleDiff / 2.0
                mp'' <- drawBushList bushes mp' canvas endPointX endPointY startAngle angleDiff
                return mp''
        else do mp'' <- drawBushList [] mp' canvas 0 0 0 0
                return mp''
drawBush a mp _ _ _ _ = do
    return mp

drawBushList [] mp _ _ _ _ _ = do return mp
drawBushList (bush:bushes) mp canvas startPointX startPointY angle angleDiff= do
    mp' <- drawBush bush mp canvas startPointX startPointY angle
    mp''<- drawBushList bushes mp' canvas startPointX startPointY (angle+angleDiff) angleDiff
    return mp''

drawBranch branch mp canvas startPointX startPointY endPointX endPointY= do
    let id = fst branch
    let color = getRGB $ snd branch
    drawWin <- widgetGetDrawWindow canvas
    renderWithDrawable drawWin $ do
        setSourceRGB (r color) (g color) (b color)
        setLineWidth 6
        setLineCap LineCapRound
        setLineJoin LineJoinMiter

        moveTo startPointX startPointY
        lineTo endPointX endPointY
        stroke

    let mp' = updateMapMultiple 7 startPointX startPointY endPointX endPointY id mp
    return mp'

getEndPoints x y angle length = (x2, y2) where
    radians = getRadians angle
    x2 = x + length * (cos radians)
    y2 = y + length * (sin radians)

updateMapMultiple :: Double -> Double -> Double -> Double -> Double -> Integer -> HM -> HM
updateMapMultiple t x1 y1 x2 y2 id mp
    | t < 0 = mp
    | otherwise = updateMapMultiple (t-1) x1 y1 x2 y2 id $ updateMap (x1+t) y1 (x2+t) y2 id mp

updateMap :: Double -> Double -> Double -> Double -> Integer -> HM -> HM
updateMap x1 y1 x2 y2 id mp =
    let yDiff = y2-y1 
        xDiff = x2-x1
        points =  if (abs yDiff) <= (abs xDiff) 
                      then let m = (y2 - y1) / (x2-x1)
                      in if (x2-x1) >= 0 then [ ((x1 + x, y1 + x*m),id) | x <- [ (x2 -x1), (x2 - x1 - 1) .. 0] ]
                         else  [ ((x2 + x, y2 + x*m),id) | x <- [ (x1 -x2), (x1 - x2 - 1) .. 0] ]
                  else let m = (x2 - x1) / (y2-y1)
                      in if (y2-y1) >= 0 then [ ((x1 + m*y, y1 + y),id) | y <- [ (y2 - y1), (y2 - y1 - 1) .. 0] ]
                         else [ ((x2 + m*y, y2 + y),id) | y <- [ (y1 - y2), (y1 - y2 - 1) .. 0] ]
    in insertIntoMap mp points

insertIntoMap :: HM -> [((Double, Double) ,Integer)] -> HM
insertIntoMap mp [] = mp
insertIntoMap mp (idPoint:idPoints) =
    let point = fst idPoint
        rndX = round $ fst point
        rndY = round $ snd point
        rndP = (rndX, rndY)
    in M.insert rndP (snd idPoint) $ insertIntoMap mp idPoints

getRGB branchColor
    | branchColor == Red = RGB 1 0 0
    | branchColor == Green = RGB 0 1 0
    | branchColor == Blue = RGB 0 0 1
    | branchColor == White = RGB 1 1 1

getRadians degrees = radians where
    radians = (degrees * pi) / 180

{-

newGame gui mp= do
    let difficulty = 2
    let players = 2
    g <- newStdGen
    let randTuple = randomNumber g 2 (3*difficulty)
    let numberOfBushes = fst randTuple
    let g' = snd randTuple
    let board = randomBoard g' numberOfBushes difficulty 0
    displayBoard gui board mp 0

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
-}

randomColor :: RandomGen g => g -> (BranchColor, g)
randomColor g = case randomR (0,2) g of (r, g') -> (toEnum r, g')

randomNumber :: RandomGen g => g -> Integer -> Integer -> (Integer, g)
randomNumber g st en = do
    randomR (st::Integer, en::Integer) g

--randomBushTuple :: (Num a, Ord a, RandomGen g) => g -> a -> Integer -> (Bush BranchObject, g)
randomBushTuple g h difficulty count
    | h <= 0 = (Empty, g)
    | otherwise = let randColorTuple = randomColor g
                      branchObject = ( count , fst randColorTuple )
                      g' = snd randColorTuple
                      randTuple = randomNumber g' 0 (2*difficulty-1)
                      numberOfSubBushes = fst randTuple
                      g'' = snd randTuple
                      bush = Branch branchObject $ randomBushList g'' (h-1) numberOfSubBushes difficulty $ count+1
                  in (bush, g'')

randomBushList :: (Num a, Ord a, RandomGen g) => g -> a -> Integer -> Integer -> Integer -> Board
randomBushList _ _ 0 _ _ = []
randomBushList g h numberOfSubBushes difficulty count = 
    let bushTuple = randomBushTuple g (h-1) difficulty count
        bush = fst bushTuple
        len = (findMaxId bush) + 1
        g' = snd bushTuple
    in deleteAllInstances Empty (bush : randomBushList g' h (numberOfSubBushes-1) difficulty (count+len))

randomBoard :: (Num a, Ord a, RandomGen g) => g -> a -> Integer -> Integer -> Board
randomBoard g d difficulty count
    | d<=0 = []
    | otherwise = let randTuple = randomNumber g 1 (2*difficulty + 1)
                      height = fst randTuple
                      g' = snd randTuple
                      bushTuple = randomBushTuple g' height difficulty count
                      bush = fst bushTuple
                      len = (findMaxId bush) + 1
                      g'' = snd bushTuple
                  in bush : randomBoard g'' (d-1) difficulty (count+len)

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

flattenBushList [] = []
flattenBushList (bush:bushes) = (flattenBush bush) ++ (flattenBushList bushes)
flattenBush (Branch a bushes)= a : (flattenBushList bushes)

findMaxIdList [] = 0
findMaxIdList (bush:bushes) = max (findMaxId bush) (findMaxIdList bushes)
findMaxId (Branch a bushes)= max (fst a) (findMaxIdList bushes)
findMaxId Empty = -1

findLength bush = toInteger $ length (flattenBush bush)

findInBushList [] _ = White
findInBushList (bush:bushes) id = if (findInBush bush id) /= White then (findInBush bush id) else findInBushList bushes id
findInBush (Branch branch bushes) id = if id == (fst branch) then (snd branch) else findInBushList bushes id
findInBush Empty _ = White

findColorInBushList [] _ = False
findColorInBushList (bush:bushes) color = if (findColorInBush bush color) then True else findColorInBushList bushes color
findColorInBush (Branch branch bushes) color = if color == (snd branch) then True else findColorInBushList bushes color
findColorInBush Empty _ = False

removeBushList id [] = []
removeBushList id (bush:bushes) = removeBush id bush : removeBushList id bushes
removeBush id (Branch b bushes) = if(id==(fst b)) then Empty else Branch b $removeBushList id bushes
removeBush _ Empty = Empty

cut [] _ =[]
cut (bush:bushes) id =
    let newBush = if(findInBush bush id) /= White
                    then (removeBush id bush) : bushes
                    else bush : cut bushes id
    in deleteAllInstances Empty newBush

cutBush bushes id = deleteAllInstances Empty (cut bushes id)

getInteger :: String -> IO Integer
getInteger string = do
    let integer = read string :: Integer
    return integer

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    getLine