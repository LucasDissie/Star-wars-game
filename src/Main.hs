module Main where

import Controller
import Model
import View
import GameLoop
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

main :: IO ()
main = do 
  playerSprite          <- loadBMP "sprites/spaceship.bmp"
  asteroidSprite        <- loadBMP "sprites/asteroid.bmp"
  milleniumFalconSprite <- loadBMP "sprites/MilleniumFalcon.bmp"
  tieFighterSprite      <- loadBMP "sprites/TieFighter.bmp"
  destoyerSprite        <- loadBMP "sprites/Destroyer.bmp"
  blinkSprite           <- loadBMP "sprites/Blink1.bmp"
  bulletSprite          <- loadBMP "sprites/Bullet.bmp"
  menu                  <- loadBMP "sprites/Menu.bmp"
  laserSprite           <- loadBMP "sprites/Bullet.bmp"
  let sprites = Sprites playerSprite asteroidSprite tieFighterSprite destoyerSprite milleniumFalconSprite blinkSprite bulletSprite laserSprite menu
  generator <- getStdGen
  playIO (InWindow "Counter" (screenWidth, screenHeight) (0, 0)) -- Or FullScreen
    black                             -- Background color
    60                                -- Frames per second
    (initialMenuState sprites generator)  -- Initial state
    view                              -- View function
    input                             -- Event function
    step                              -- Step function