module Randomness where
import Model
import System.Random
import Control.Monad.State.Lazy

getRandomEnemies :: Int -> State GameState ()
getRandomEnemies difficulty = do
  i <- getRandomNumber (0,difficulty)
  spawn <- getSpawn
  case i of  -- every difficulty level will increase the chance of a harder enemy to spawn
    0 -> do
      s <- getRandomStone spawn
      modify (\gstate -> gstate{stones = s : stones gstate })
    1 -> do
      b <- getRandomBlink spawn
      modify (\gstate -> gstate{blinks = b : blinks gstate})
    2 -> do
      ts <- getRandomTieFighter spawn
      modify (\gstate -> gstate{tieFighters = ts : tieFighters gstate})
    3 -> do
      ds <- getRandomDestroyer spawn
      modify (\gstate -> gstate{destroyers = ds : destroyers gstate})
    4 -> do
      spawnEnemies 2
    5 -> do
      spawnEnemies 3
    6 -> do
      getRandomEnemies 5   -- maybe spawn more enemies, but at least 3
      spawnEnemies 2
    _ -> do
      getRandomEnemies 6   -- maybe spawn way more enemies, but atleast 4
      spawnEnemies 4
  where spawnEnemies 0 = return ()
        spawnEnemies i = do
          getRandomEnemies 3
          spawnEnemies (i - 1)

    




getRandomTieFighter :: (Float,Float,Float) -> State GameState TieFighter
getRandomTieFighter (s,p,sp) = do
  let size = s * 40 + 20
  let position = (p - 0.5) * (1000 - size)
  let speed = sp * 200 + 300 
  state (\gstate -> (TieFighter (Rect 800 position size size) speed ((fromIntegral (round size) :: Integer) * 100) 0,gstate))


getRandomBlink :: (Float,Float,Float) -> State GameState Blink
getRandomBlink (s,p,sp) = do
  let size = s * 40 + 20
  let position = (p - 0.5) * (1000 - size)
  let speed = sp * 150 + 150 
  bm <- getRandomFloat
  let blinkmult = bm * 2 + 1
  state (\gstate -> (Blink (Rect 800 position size size) speed 0 blinkmult size ((fromIntegral (round size) :: Integer) * 60),gstate))


getRandomStone :: (Float,Float,Float) -> State GameState Stone
getRandomStone (s,p,sp) = do
  let size = s * 60 + 20
  let position = (p - 0.5) * (1000 - size)
  let speed = sp * 200 + 200 
  rs <- getRandomFloat
  let rotspeed = (rs - 0.5) * 10
  state (\gstate -> (Stone (Rect 800 position size size) speed 0 rotspeed ((fromIntegral (round size) :: Integer) * 40),gstate))

getRandomDestroyer :: (Float,Float,Float) -> State GameState Destroyer
getRandomDestroyer (s,p,sp) = do
  let size = s * 80 + 20
  let position = (p - 0.5) * (1000 - size)
  let speed = sp * 100 + 100 
  state (\gstate -> (Destroyer (Rect 800 position (2.7 * size) size) speed ((fromIntegral (round size) :: Integer) * 200) 3,gstate))

getSpawn :: State GameState (Float,Float,Float)
getSpawn = do
  s <- getRandomFloat 
  p <- getRandomFloat 
  sp <- getRandomFloat
  state (\gstate -> ((s,p,sp),gstate))
  

getRandomFloat :: State GameState Float
getRandomFloat = state randomFloat
randomFloat :: GameState -> (Float,GameState)
randomFloat gstate@Playing {randomGen = gen} = let (f,gen') = random gen :: (Float,StdGen)
                                               in (f,gstate{randomGen = gen'})

getRandomNumber :: (Int,Int) -> State GameState Int
getRandomNumber range = state (randomNumber range)
randomNumber :: (Int,Int) -> GameState -> (Int,GameState)
randomNumber range gstate@Playing {randomGen = gen} = let (i,gen') = randomR range gen :: (Int,StdGen)
                                                      in (i,gstate{randomGen = gen'})