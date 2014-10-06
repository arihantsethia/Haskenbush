import Data.Char
import Data.List
import Data.Maybe
import Graphics.UI.Gtk hiding (Cross)
import Graphics.UI.Gtk.Glade
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.GC
import Prelude
import System.IO
import System.Random
import Data.HashMap as M

type HM = M.Map (Integer,Integer) Integer
type BranchObject = (Integer, BranchColor)
type Board = [Bush BranchColor]
data BranchColor = Red | Green | Blue deriving (Read, Show, Enum, Eq, Ord)
data Bush a = Empty | Leaf a | Branch a [Bush a] deriving (Read, Show, Eq, Ord)

players :: [String]
players = []

branchLength = -50

data GUI = GUI {
    mainWindow :: Window,
    gameBoard :: DrawingArea,
    gameStatus :: Statusbar,
    mNewGame :: MenuItem,
    mQuitGame :: MenuItem,
    help :: MenuItem,
    playDialog :: Dialog,
    bPlayOk :: Button,
    difficultyDialog :: Dialog,
    cbDifficulty :: ComboBox,
    bDiffOk :: Button
--    awEntry :: Entry
}

data RGB = RGB {
    r :: Double,
    g :: Double,
    b :: Double
}

main :: IO ()
main = do
    initGUI

    gui <- loadGlade "haskenbush.glade"
    let mp = M.empty :: HM
    mp' <- newGame gui mp
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

loadGlade gladepath =
    do
       Just xml <- xmlNew gladepath
       mw <- xmlGetWidget xml castToWindow "mWindow"
       pDrawArea <- xmlGetWidget xml castToDrawingArea "gameBoard"
       sBar <- xmlGetWidget xml castToStatusbar "gameStatus"
       nGame <- xmlGetWidget xml castToMenuItem "newGame"
       q <- xmlGetWidget xml castToMenuItem "quit"
       h <- xmlGetWidget xml castToMenuItem "help"
       [bPlayOk, bDiffOk] <- mapM (xmlGetWidget xml castToButton) ["bPlayOk", "bDiffOk"]
       pDialog <- xmlGetWidget xml castToDialog "playDialog"
       dDialog <- xmlGetWidget xml castToDialog "difficultyDialog"
       cBox <- xmlGetWidget xml castToComboBox "cbDifficulty"
       return $ GUI mw pDrawArea sBar nGame q h pDialog bPlayOk dDialog cBox bDiffOk

connectGui gui = do
    onDestroy (mainWindow gui) mainQuit

displayBoard gui board mp = do
    let canvas = (gameBoard gui)
    let numberOfBushes = fromIntegral $ (length board)
    let divLength =  900 / numberOfBushes
    let startPointX = 50 + divLength/2
    let startPointY = 550
    print board
    let endPoints = getEndPoints startPointX startPointY 90 branchLength
    widgetModifyBg canvas StateNormal (Color 65535 65535 65535)
    widgetShowAll (mainWindow gui)
    drawin <- widgetGetDrawWindow canvas
    mp' <- renderWithDrawable drawin $ drawBoard board startPointX startPointY divLength mp
    --print mp'
    onExpose canvas (\x -> do 
        renderWithDrawable drawin $ drawBoard board startPointX startPointY divLength mp
        return False)
    canvas `onButtonPress` mouseClick canvas mp'
    return mp'

mouseClick :: DrawingArea -> HM -> event -> IO Bool
mouseClick can mp _evt =
    do p <- widgetGetPointer can
       putStrLn ("clicked: " ++ show p)
       let x = fromIntegral $ fst p
       let y = fromIntegral $ snd p
       let z  = getId mp x y
       print z
       return True

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

drawBoard :: [Bush BranchObject] -> Double -> Double -> Double -> HM -> Render HM
drawBoard [] _ _ _ mp = do
    stroke
    return mp
drawBoard (bush:board) startPointX startPointY divLength mp= do
    mp' <- drawBush bush startPointX startPointY 90 mp
    mp'' <- drawBoard board (startPointX + divLength) startPointY divLength mp'
    return mp''

drawBushList :: [Bush BranchObject] -> Double -> Double -> Double -> Double -> HM -> Render HM
drawBushList [] _ _ _ _ mp = do 
    stroke
    return mp
drawBushList (bush:bushes) startPointX startPointY angle angleDiff mp = do
    mp' <- drawBush bush startPointX startPointY angle mp
    mp''<- drawBushList bushes startPointX startPointY (angle+angleDiff) angleDiff mp'
    return mp''

drawBush :: Bush BranchObject -> Double -> Double -> Double -> HM -> Render HM
drawBush (Branch a bushes) startPointX startPointY angle mp = do
    let endPoints = getEndPoints startPointX startPointY angle branchLength
    let endPointX = fst endPoints
    let endPointY = snd endPoints
    mp' <- drawBranch a startPointX startPointY endPointX endPointY mp
    let numberOfSubBushes = fromIntegral $ length bushes
    if numberOfSubBushes > 0 
        then do let angleDiff = 180.0 / numberOfSubBushes
                let startAngle = angle - 90.0 + angleDiff / 2.0
                mp'' <- drawBushList bushes endPointX endPointY startAngle angleDiff mp'
                return mp''
        else do mp'' <- drawBushList [] 0 0 0 0 mp'
                return mp''

drawBranch :: BranchObject -> Double -> Double -> Double -> Double -> HM -> Render HM
drawBranch branch startPointX startPointY endPointX endPointY mp= do
    let id = fst branch
    let color = getRGB $ snd branch
    setSourceRGB (r color) (g color) (b color)
    setLineWidth 6

    moveTo startPointX startPointY
    lineTo endPointX endPointY
    let mp' = updateMapMultiple 7 startPointX startPointY endPointX endPointY id mp
    stroke
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

getRadians degrees = radians where
    radians = (degrees * pi) / 180

makeMenuBar = do
  mb <- menuBarNew
  fileMenu <- menuNew
  newGame1 <- menuItemNewWithMnemonic "_New Game"
  quit1 <- menuItemNewWithMnemonic "_Quit"
  file1 <- menuItemNewWithMnemonic "_Game"
  menuShellAppend fileMenu newGame1
  menuShellAppend fileMenu quit1
  menuItemSetSubmenu file1 fileMenu
  containerAdd mb file1
  return (mb,newGame1,quit1)

newGame gui mp= do
    let difficulty = 2
    let players = 2
    g <- newStdGen
    let randTuple = randomNumber g 2 (3*difficulty)
    let numberOfBushes = fst randTuple
    let g' = snd randTuple
    let board = randomBoard g' numberOfBushes difficulty 0
    displayBoard gui board mp

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

--randomBushList :: (Num a, Ord a, RandomGen g) => g -> a -> Integer -> Integer -> [Bush BranchObject]
randomBushList _ _ 0 _ _ = []
randomBushList g h numberOfSubBushes difficulty count = 
    let bushTuple = randomBushTuple g (h-1) difficulty count
        bush = fst bushTuple
        len = (findMaxId bush) + 1
        g' = snd bushTuple
    in deleteAllInstances Empty (bush : randomBushList g' h (numberOfSubBushes-1) difficulty (count+len))

flattenBushList [] = []
flattenBushList (bush:bushes) = (flattenBush bush) ++ (flattenBushList bushes)
flattenBush (Branch a bushes)= a : (flattenBushList bushes)

findMaxIdList [] = 0
findMaxIdList (bush:bushes) = max (findMaxId bush) (findMaxIdList bushes)
findMaxId (Branch a bushes)= max (fst a) (findMaxIdList bushes)

findLength bush = toInteger $ length (flattenBush bush)

randomBoard :: (Num a, Ord a, RandomGen b) => b -> a -> Integer -> Integer -> [Bush BranchObject]
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

getInteger :: String -> IO Integer
getInteger string = do
    let integer = read string :: Integer
    return integer

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    getLine