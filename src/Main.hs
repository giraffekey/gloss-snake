module Main where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

-- Data Structures

data Direction = NORTH | EAST | SOUTH | WEST

instance Eq Direction where
  x == y =
    toInt x == toInt y
    where
      toInt a =
        case a of
          NORTH -> 0
          EAST  -> 1
          SOUTH -> 2
          WEST  -> 3

data World = World
  { screenDims :: (Int, Int)
  , size  :: (Int, Int)
  , snake :: [(Int, Int)]
  , apple :: (Int, Int)
  , dir   :: Direction
  , step  :: Float
  , seed  :: StdGen
  }

-- Game Logic

randomPosition :: (Int, Int) -> StdGen -> ((Int, Int), StdGen)
randomPosition (w, h) seed =
  let
    (x, seed')  = randomR (0, w-1) seed
    (y, seed'') = randomR (0, h-1) seed'
  in
  ((x, y), seed'')

makeWorld :: (Int, Int) -> (Int, Int) -> StdGen -> World
makeWorld screenDims size seed =
  let 
    (snake, seed')  = randomPosition size seed
    (apple, seed'') = randomPosition size seed' 
  in
  World
    { screenDims = screenDims
    , size  = size
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
    eatApple = snake world !! 0 == apple world
    tick = 1.0 / fromIntegral ((length (snake world) `div` 5) + 1)
    snake' =
      if step world > tick then
        nextSnake (snake world) (dir world)
      else
        snake world
    snake'' =
      if eatApple then
        let 
          (i, j) = snake' !! (length snake' - 1)
          tail =
            case dir world of
              NORTH -> (i, j + 1)
              EAST  -> (i - 1, j)
              SOUTH -> (i, j - 1)
              WEST  -> (i + 1, j)
        in
        snake' ++ [tail]
      else
        snake'
    step' =
      if step world > tick then
        step world + dt - tick
      else
        step world + dt
    (apple', seed') =
      if eatApple then
        randomPosition (size world) (seed world)
      else
        (apple world, seed world)
  in
  world
    { snake = snake''
    , step  = step'
    , apple = apple'
    , seed  = seed'
    }

-- Drawing

drawWorld :: World -> Picture
drawWorld world =
  let pics = tile appleSqr (apple world) : map (tile snakeSqr) (snake world) in
  scale (fromIntegral screenW / fromIntegral w) (-(fromIntegral screenH) / fromIntegral h) $
  translate (-(fromIntegral w)/2 + 0.5) (-(fromIntegral h)/2 + 0.5) $
  pictures pics
  where
    (screenW, screenH) = screenDims world
    (w, h)   = size world
    snakeSqr = color green (rectangleSolid 1 1)
    appleSqr = color red (rectangleSolid 1 1)
    tile sq (i, j) = translate (fromIntegral i) (fromIntegral j) sq

-- Events

handleEvents :: Event -> World -> World

handleEvents (EventKey (SpecialKey KeyUp)    Down _ _) world = if dir world == SOUTH then world else world {dir = NORTH}
handleEvents (EventKey (SpecialKey KeyDown)  Down _ _) world = if dir world == NORTH then world else world {dir = SOUTH}
handleEvents (EventKey (SpecialKey KeyLeft)  Down _ _) world = if dir world == EAST  then world else world {dir = WEST}
handleEvents (EventKey (SpecialKey KeyRight) Down _ _) world = if dir world == WEST  then world else world {dir = EAST}

handleEvents _ world = world

-- Main

main :: IO ()
main = do
  seed <- newStdGen
  let screenDims = (500, 500)
  let world = makeWorld screenDims (10, 10) seed
  let window = InWindow "Snake" screenDims (100, 100)
  play window black 60 world drawWorld handleEvents nextWorld
