module Main where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

-- Data Structures

data Direction = NORTH | EAST | SOUTH | WEST

data World = World
  { screenDims :: (Int, Int)
  , size :: (Int, Int)
  , snake :: [(Int, Int)]
  , apple :: (Int, Int)
  , dir :: Direction
  , step :: Float
  , seed :: StdGen
  }

-- Game Logic

randomPosition :: (Int, Int) -> StdGen -> ((Int, Int), StdGen)
randomPosition (w, h) seed =
  let (x, seed') = randomR (0, w-1) seed in
  let (y, seed'') = randomR (0, h-1) seed' in
  ((x, y), seed'')

makeWorld :: (Int, Int) -> (Int, Int) -> StdGen -> World
makeWorld screenDims size seed =
  let (snake, seed') = randomPosition size seed in
  let (apple, seed'') = randomPosition size seed' in
  World
    { screenDims = screenDims
    , size = size
    , snake = [snake]
    , apple = apple
    , dir =
        if snd snake < snd size `div` 2 then
          EAST
        else
          WEST
    , step = 0
    , seed = seed''
    }

nextSnake :: [(Int, Int)] -> Direction -> [(Int, Int)]
nextSnake snake dir =
  let
    head = nextHead (snake !! 0) dir
    tail = [snake !! i | i <- [0..length snake - 2]]
  in
  head : tail
  where
    nextHead (i, j) NORTH = (i, j - 1)
    nextHead (i, j) EAST = (i + 1, j)
    nextHead (i, j) SOUTH = (i, j + 1)
    nextHead (i, j) WEST = (i - 1, j)

nextWorld :: Float -> World -> World
nextWorld dt world =
  let
    newSnake =
      if step world > 1 then
        nextSnake (snake world) (dir world)
      else
        snake world
    newStep =
      if step world > 1 then
        step world + dt - 1
      else
        step world + dt
  in
  world {snake = newSnake, step = newStep}

-- Drawing

drawWorld :: World -> Picture
drawWorld world =
  let pics = tile appleSqr (apple world) : map (tile snakeSqr) (snake world) in
  scale (fromIntegral screenW / fromIntegral w) (-(fromIntegral screenH) / fromIntegral h) $
  translate (-(fromIntegral w)/2 + 0.5) (-(fromIntegral h)/2 + 0.5) $
  pictures pics
  where
    (screenW, screenH) = screenDims world
    (w, h) = size world
    snakeSqr = color green (rectangleSolid 1 1)
    appleSqr = color red (rectangleSolid 1 1)
    tile sq (i, j) = translate (fromIntegral i) (fromIntegral j) sq

-- Events

handleEvents :: Event -> World -> World
handleEvents _ world = world

-- Main

main :: IO ()
main = do
  seed <- newStdGen
  let screenDims = (500, 500)
  let world = makeWorld screenDims (10, 10) seed
  let window = InWindow "Snake" screenDims (100, 100)
  play window black 20 world drawWorld handleEvents nextWorld
