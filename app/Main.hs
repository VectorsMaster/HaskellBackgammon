module Main where

import qualified MyLib (someFunc)

import Graphics.Gloss

import Graphics.Gloss.Interface.Pure.Game

data World = Point { x :: Float, y :: Float } deriving (Eq, Show)

main :: IO ()
main = play
  (InWindow "Move the Circle" (400, 400) (10, 10))
  white
  30
  (Point 0 0)
  draw
  handleInput
  update

draw :: World -> Picture
draw (Point x' y') = translate x' y' (Color red (circle 50))

handleInput :: Event -> World -> World
handleInput (EventKey (SpecialKey KeyUp) Down _ _) (Point x' y') = Point x' (y' + 10)
handleInput (EventKey (SpecialKey KeyDown) Down _ _) (Point x' y') = Point x' (y' - 10)
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) (Point x' y') = Point (x' - 10) y'
handleInput (EventKey (SpecialKey KeyRight) Down _ _) (Point x' y') = Point (x' + 10) y'
handleInput _ world = world

update :: Float -> World -> World
update _ world = world
