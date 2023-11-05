module Main where

import MyLib
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game ( Event )




-- | Represents a sequence of moves (in their order) made by ONE PLAYER in ONE TURN
-- | Each elements in the sequence represents one move and consists of pair of integers
-- | Let the pair be (X, Y)
-- | X: Represents the index of the triangle from where the character to be moved is chosen.
-- | Y: Represents the number of forward points of which the character to be moved.
-- | The sequence can be of of three different sizes (1,2 , 4)
type Move = [(Int, Int)]


-- | The first integer represnts the color of pieces in this triangle it can be 0 or 1
-- | The Second integer represents the number of pieces in this triangle from it can be from 0 to 15
-- | The last integer represents "mahbousa" it can be 1 if this triangle contains one piece of the other color which cannot move and 0 otherwise
type GameTriangle = (Int, Int, Int)

-- | the dirction of triangles and pieces
data Direction = Upward | Downward

-- | dices : the values of the dices in the current turn
-- | turn  : can be either 0 or 1
-- | triangles : the state of triangles according to the player who will play in this turn. it has exactly 24 elements.
-- | availableMoves : all valid sequence of moves that the player can play in this turn according to state of the game.
data World = World {
    dices :: (Int, Int),
    turn :: Int,
    triangles :: [GameTriangle],
    availableMoves :: [Move],
    randoms :: [Int],
    bearingOff :: Bool

} deriving (Eq, Show)

main :: IO ()
main = display
    (InWindow "Backgammon" (700, 650) (10, 10))
    white
    (drawBoard initialWorld)

-- main = play
--     (InWindow "Backgammon" (1000, 1000) (100, 100))
--     white
--     100
--     initialWorld
--     drawBoard
--     handleInput
--     (\_ world -> world)

-- | Change the state of the game according to user's input
-- handleInput :: Event -> World -> World
-- handleInput event world = _toDo

initialTriangles :: [(Int, Int, Int)]
initialTriangles = [
    (1, 15, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0),(0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0),(0, 0, 0),(0, 0, 0), (0, 0, 0),
     (0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0),(0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0),(0, 0, 0),(0, 0, 0), (0, 15, 0)
    ]

initialWorld :: World
initialWorld = World (1, 6) 0 initialTriangles [] MyLib.myArray False

brown :: Color
brown = makeColor 0.4375 0.30859375 0.1640625 1.0

drawPiece :: Color -> Picture
drawPiece color = Color color $ ThickCircle 5 10

-- | Render the State of the Game.
drawBoard :: World -> Picture
drawBoard world = translate (-330) 220 (dice <> table <> downwardTriangles <> shiftedUpwardTriangles)
    where
        len = length (triangles world)
        height = 500
        break = 20
        downwardTriangles = half1Downwards <> translate (50*6 + break) 0 half2Downwards
        upwardTriangles =  half1Upwards <> translate (-50*6 - break) 0 half2Upwards
        quarter = len `div` 4
        table = translate (50 * fromIntegral quarter + 10) (-height/2) (Color brown $ rectangleSolid (50*(fromIntegral quarter * 2) + break) 500)

        half1Downwards = drawGameTriangles (take quarter (triangles world)) Downward
        half1Upwards = drawGameTriangles (takeRange (quarter * 2) (quarter * 3 - 1) (triangles world)) Upward

        half2Downwards = drawGameTriangles (takeRange quarter (quarter * 2 - 1) (triangles world)) Downward
        half2Upwards = drawGameTriangles (takeRange (quarter * 3) len (triangles world)) Upward

        shiftedUpwardTriangles = translate (50*(fromIntegral quarter * 2 - 1) + break) (-height) upwardTriangles
        currentDice = dices world
        dice = if getFirstElement currentDice /= (-1) then Text (show currentDice) else blank

getFirstElement :: (a, b) -> a
getFirstElement (x, _) = x


-- Draw game triangles
drawGameTriangles :: [GameTriangle] -> Direction -> Picture
drawGameTriangles [] _ = blank
drawGameTriangles (triangle:triangles) direction = firstTriangle <> remainingTriangles
    where
        shift = case direction of
            Upward -> -50
            Downward -> 50
        firstTriangle = drawGameTriangle triangle direction
        remainingTriangles = translate shift 0 (drawGameTriangles triangles direction)

drawPieces :: GameTriangle -> Direction -> Picture
drawPieces triangle direction = translate 25 y (mahbousaPiece <> translate 0 yMahbousa (drawIdenticalPieces numPieces direction piecesColor))
    where
        y = case direction of
            Upward -> 10
            Downward -> -10
        (color, numPieces, mahbousa) = triangle
        mahbousaColor = if color == 1 then black else white
        piecesColor = if color == 0 then black else white
        mahbousaPiece = if mahbousa == 0 then blank else drawPiece mahbousaColor
        yMahbousa = if mahbousa == 0 then 0 else y

drawIdenticalPieces :: Int -> Direction -> Color -> Picture
drawIdenticalPieces 0 direction color = blank
drawIdenticalPieces numPieces direction color = piece <> translate 0 y pieces
    where
        y = case direction of
            Upward -> 15
            Downward -> -15
        piece = drawPiece color
        pieces = drawIdenticalPieces (numPieces - 1) direction color

-- Draw a single game triangle
drawGameTriangle :: GameTriangle -> Direction -> Picture
drawGameTriangle triangle direction = directedTriangle <> circles
    where
        directedTriangle = Color red $ case direction of
            Upward -> Polygon [(0,0), (25,100), (50,0)]
            Downward -> Polygon [(0,0), (25,-100), (50,0)]
        circles = drawPieces triangle direction

takeRange :: Int -> Int -> [a] -> [a]
takeRange i j l = take (j-i+1) (drop i l)

colorExist :: GameTriangle -> Int -> Bool
colorExist (x, y, _) clr = x == clr && y > 0

inv :: Int -> Int
inv 0 = 1
inv _ = 0

changeTriangle :: GameTriangle -> Int -> Int -> GameTriangle
changeTriangle (x, y, z) val clr
    | y == 1 && x /= clr      = (clr, 1, 1)
    | y + val == 0 && z == 1  = (inv clr, 1, 0)
    | y + val == 0            = (0, 0, 0)
    | otherwise               = (clr, y + val, z)

changeTriangles :: [GameTriangle] -> Int -> Int -> Int -> [GameTriangle]
changeTriangles lst id val trn = iter lst val id trn 0
    where
        iter :: [GameTriangle] -> Int -> Int -> Int -> Int -> [GameTriangle]
        iter [] _ _ _ _ = []
        iter (x:xs) v i t pos = if pos == i then changeTriangle x v trn : xs else x : iter xs v i t (pos + 1)

-- | removing or adding number of pieces to a specific triangle in the world
changeWorld :: World -> Int -> Int -> World
changeWorld (World d trn t a r b) id val = World d trn (changeTriangles t id val trn) a r b

fndColor :: GameTriangle -> Int
fndColor (x, _, _) = x

fndCount :: GameTriangle -> Int
fndCount (_, y, _) = y

allInHalf :: [GameTriangle] -> Int -> Bool
allInHalf lst trn = iter lst 0
    where
        check (x, y, z)
            | x == trn && y > 0 = True
            | x /=trn && z == 1 = True
            | otherwise         = False

        iter [] _ = True
        iter (x:xs) pos = not (pos >= 12 && check x) && iter xs (pos + 1)

updateBearingOff :: World -> World
updateBearingOff (World d trn t a r b) = if allInHalf t trn then World d trn t a r True else World d trn t a r False

-- | First parameter (x, y)
-- | X: Represents the index of the triangle from where the character to be moved is chosen.
-- | Y: Represents the number of forward points of which the character to be moved.
-- | Second parameter the current state of the game.
-- | It returns (True, newState) if this move is valid
-- | otherwise it returns (False, curState)
isValidMove :: (Int, Int) -> World -> (Bool, World)
isValidMove (x, y) world
    | not (colorExist currentTriangle currentTurn) = (False, world)
    | x - y < 0  && not (bearingOff world)         = (False, world)
    | x - y < 0                                    = (True, changeWorld world x (-1))
    | desCount == 0 || desColor == currentTurn     = (True, changeWorld (changeWorld world x (-1)) (x - y) 1)
    | desCount == 1                                = (True, changeWorld (changeWorld world x (-1)) (x - y) 1)
    | otherwise                                    = (False, world)
        where
            currentTriangle = triangles world !! x
            currentTurn = turn world
            desTriangle = triangles world !! (x - y)
            desCount = fndCount desTriangle
            desColor = fndColor desTriangle


-- | Checks if a sequence of moves to be played by one player in this turn is valid or not using isValidMove function.
isValidMovesSequence :: Move -> World -> (Bool, World)
isValidMovesSequence [] world = (True, world)
isValidMovesSequence lst world = iter lst world 
    where 
        iter [] curWorld = (True, curWorld)
        iter (x:xs) curWorld 
            | ok        = iter xs nextWorld
            | otherwise = (False, curWorld)
                where 
                    (ok, newWorld) = isValidMove x world
                    nextWorld = updateBearingOff newWorld

-- | Returns a list of all moves according to the numbers in the dices (valid and not valid)
-- getAllMoves :: World -> [Move]
-- getAllMoves = _toDo

-- -- | filter all Moves resulted from getAllMoves and assign the valid of them to the availableMoves member in World Struct
-- getValidMoves :: World -> World
-- getValidMoves = _toDo


handleState :: Move -> World -> World
handleState move world = world

-- -- | generate to random numbers from 1 to 6 and assign them the dices member in World struct
throwDices :: World -> World
throwDices (World (-1, -1) turn triangles availableMoves (r1:(r2:rs)) b) = World (r1, r2) turn triangles availableMoves rs b
throwDices world = world