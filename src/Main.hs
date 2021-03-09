module Main where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

-- Data Structures

data World = World
  { screenDims :: (Int, Int)
  , size :: (Int, Int)
  , snake :: [(Int, Int)]
  , apple :: (Int, Int)
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
    , seed = seed''
    }

nextWorld :: Float -> World -> World
nextWorld _ world = world

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
  play window black 60 world drawWorld handleEvents nextWorld
