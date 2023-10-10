-- | This module contains the data types
--   which represent the state of the game
module Model where

import Control.Monad.State.Lazy
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

screenWidth = 1600 :: Int
screenHeight = 1000 :: Int
screenrect = Rect 0 0 ((fromIntegral screenWidth :: Float) + 200) ((fromIntegral screenHeight :: Float) + 200)
initialPlayerRect = Rect (fromIntegral ((-screenWidth) `div` 2 + 100) :: Float) 0 90 96
type KeyStatus = KeyState
data KeyPress = Key { key    :: Char,
                      status :: KeyStatus} 

data Player = Player{playerInput :: [Char], playerPosition :: CollisionShape, cooldown :: Float, points :: Integer, lives :: Integer}

data Bullet = Bullet {bulletPosition :: CollisionShape, bulletSpeed :: Float}

data Sprites = Sprites{playerSprite :: Picture, asteroidSprite :: Picture, tieFighterSprite :: Picture, destroyerSprite :: Picture, milleniumFalconSprite :: Picture, blinkSprite :: Picture, bulletSprite :: Picture, laserSprite :: Picture, menuSprite :: Picture}

data Stone = Stone{stonePosition :: CollisionShape, stoneSpeed :: Float, stoneRotation :: Float, stoneRotationSpeed :: Float, stonePoints:: Integer}

data Blink = Blink{blinkPosition :: CollisionShape, blinkSpeed :: Float, blinkTime :: Float, blinkMult :: Float, blinkSize :: Float, blinkPoints :: Integer}

data TieFighter = TieFighter{tiePosition :: CollisionShape, tieSpeed :: Float, tiePoints :: Integer, tieDirection :: Float}

data Destroyer = Destroyer{desPosition :: CollisionShape, desSpeed :: Float, desPoints :: Integer, desTimer :: Float}

data Laser = Laser{laserCollider :: CollisionShape, duration :: Float}

data GameState = Gameover {score :: Integer, name :: String, written :: Bool, timer :: Float, sprites :: Sprites, randomGen :: StdGen}
                | Menu {mouse :: (Float,Float), sprites :: Sprites, randomGen :: StdGen}
                | Playing {playerState :: Player, bullets :: [Bullet], stones :: [Stone], blinks :: [Blink], tieFighters :: [TieFighter], destroyers :: [Destroyer], lasers :: [Laser],  spawnTime :: Float, playTime :: Float, randomGen :: StdGen, sprites :: Sprites} 
                | Pause GameState

setPlayer :: Player -> State GameState ()
setPlayer player = modify (\gstate -> gstate {playerState = player})

setBullets :: [Bullet] -> State GameState ()
setBullets bs = modify (\gstate -> gstate {bullets = bs})

setStones :: [Stone] -> State GameState ()
setStones ss = modify (\gstate -> gstate {stones = ss})

setBlinks :: [Blink] -> State GameState ()
setBlinks bs = modify (\gstate -> gstate {blinks = bs})

setTieFighters :: [TieFighter] -> State GameState ()
setTieFighters ts = modify (\gstate -> gstate {tieFighters = ts})

setDestroyers :: [Destroyer] -> State GameState ()
setDestroyers ds = modify (\gstate -> gstate {destroyers = ds})


data CollisionShape = Rect {x :: Float, y :: Float, w :: Float, h :: Float} 



class Drawable a where 
  draw :: Picture -> a -> Picture

class PointObject a where
  getPoints :: a -> Integer 
   
class Moveable a where
  move :: Float -> a -> a

class Shooter a where
  shoot :: Float -> a -> State GameState ()

class Collider a where
  getBox :: a -> CollisionShape
  collision :: (Collider b) => a -> b -> Bool
  collision a b = shapeCollision (getBox a) (getBox b)
  screenCollision :: a -> Bool
  screenCollision = shapeCollision screenrect . getBox

shapeCollision :: CollisionShape -> CollisionShape -> Bool
shapeCollision (Rect x1' y1' w1 h1) (Rect x2' y2' w2 h2) = 
  x1 < x2 + w2 && 
  x1 + w1 > x2 && 
  y1 < y2 + h2 && 
  y1 + h1 > y2 
  where x1 = x1' - w1 / 2
        x2 = x2' - w2 / 2
        y1 = y1' - h1 / 2
        y2 = y2' - h2 / 2


class Updateable a where
  update :: Float -> a -> a

initialMenuState :: Sprites -> StdGen -> GameState
initialMenuState = Menu (0,0)

initialPlayingState :: Sprites -> StdGen -> GameState
initialPlayingState s g = Playing {playerState = Player {playerInput = [], playerPosition = initialPlayerRect, cooldown = 0, points = 0, lives = 3}
                        , bullets = []
                        , stones = []
                        , blinks = []
                        , tieFighters = []
                        , destroyers = []
                        , lasers = []
                        , spawnTime = 1.5
                        , playTime = 0
                        , randomGen = g
                        , sprites = s
                       }

scaleToHitbox :: CollisionShape -> Picture -> Picture
scaleToHitbox a p = scale a
   where scale (Rect _ _ w h) = Scale (1 / (fst dimensions / w) ) (1 / (snd dimensions / h)) p
         getDimensions (Bitmap bd) = let (x,y) = bitmapSize bd in (fromIntegral x :: Float, fromIntegral y :: Float)
         dimensions = getDimensions p

highscores :: IO [(String,Integer)]
highscores = do 
  text <- readFile "files/highscores.txt"
  return (getScores (words text))
  where
  getScores []       = []
  getScores (x:y:xs) = (x,read y :: Integer) : getScores xs

-- instance Drawable CollisionShape where
--   draw _ (Rect x y w h) = color red (Polygon [(x - width, y - height)
--                                     ,(x + width, y - height)
--                                     ,(x + width, y + height)
--                                     ,(x - width, y + height)])
--     where width = w / 2
--           height = h / 2