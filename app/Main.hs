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
    

-- | dices : the values of the dices in the current turn
-- | turn  : can be either 0 or 1
-- | triangles : the state of triangles according to the player who will play in this turn. it has exactly 24 elements.
-- | availableMoves : all valid sequence of moves that the player can play in this turn according to state of the game.
data World = World {
    dices :: (Int, Int),
    turn :: Int,
    triangles :: [GameTriangle],
    availableMoves :: [Move],
    randoms :: [Int]
} 

main :: IO ()
main = play
    (InWindow "Backgammon" (1000, 1000) (100, 100))
    white
    100
    initialWorld
    drawBoard
    handleInput
    (\_ world -> world)

-- | Change the state of the game according to user's input
handleInput :: Event -> World -> World
handleInput event world = _toDo

initialTriangles :: [(Int, Int, Int)]
initialTriangles = [
    (1, 15, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0),(0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0),(0, 0, 0),(0, 0, 0), (0, 0, 0),
     (0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0),(0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0),(0, 0, 0),(0, 0, 0), (0, 15, 0)
    ]

initialWorld :: World
initialWorld = World (-1, -1) 0 initialTriangles [] MyLib.myArray

-- | Render the State of the Game.
drawBoard :: World -> Picture
drawBoard world = _toDo

-- | First parameter (x, y)
-- | X: Represents the index of the triangle from where the character to be moved is chosen.
-- | Y: Represents the number of forward points of which the character to be moved.
-- | Second parameter the current state of the game.
-- | It returns (True, newState) if this move is valid
-- | otherwise it returns (False, curState)
isValidMove :: (Int, Int) -> World -> (Bool, World) 
isValidMove = _toDo

-- | Checks if a sequence of moves to be played by one player in this turn is valid or not using isValidMove function.
isValidMovesSequence :: Move -> World -> (Bool, World)
isValidMovesSequence = _toDo

-- | Returns a list of all moves according to the numbers in the dices (valid and not valid)
getAllMoves :: World -> [Move]
getAllMoves = _toDo

-- | filter all Moves resulted from getAllMoves and assign the valid of them to the availableMoves member in World Struct
getValidMoves :: World -> World
getValidMoves = _toDo

-- | generate to random numbers from 1 to 6 and assign them the dices member in World struct
throwDices :: World -> World
throwDices (World (-1, -1) turn triangles availableMoves (r1:(r2:rs))) = World (r1, r2) turn triangles availableMoves rs 
throwDices world = world