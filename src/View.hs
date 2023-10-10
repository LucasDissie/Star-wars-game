-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import Player
import Bullet
import Stone
import Blink
import TieFighter
import Destroyer

view :: GameState -> IO Picture
view g@(Gameover p n w t _ _) = do
  scores <- highscores
  htext <- textHighscores scores
  let listHighScores = [Translate (-200) 100 htext,Translate (-200) 170 (scale 0.4 0.4 (color white (text "High scores:")))]
  let pics = if p /= (-100) 
                then Pictures (Translate (-200) 300 (scale 0.4 0.4 (color white (text ("Your score was: " ++ show p)))) : listHighScores)
                else Pictures listHighScores
  if not w && check scores
    then return (Pictures (pics : [Translate (-200) (-300) (scale 0.4 0.4 $ color white (text "Enter your name:")), Translate (-200) (-350) (scale 0.4 0.4 $ color white (text n))]))
    else return pics

    where check ss = p > snd (last ss)

view g = return(viewPure g)

viewPure :: GameState -> Picture
viewPure (Menu _ ss _)                        = menuSprite ss
viewPure (Playing ps b s bl ts ds ls _ _ _ ss) = Pictures (
  color white (Translate (-800)   470  (Scale 0.2 0.2 $ text ("Points: " ++ show (points ps)))) :
  color white (Translate (-800) (-490) (Scale 0.2 0.2 $ text ("Lives: "  ++ show (lives  ps)))) :
  draw (playerSprite ss)            ps : 
  map  (draw $ blinkSprite ss)      bl ++
  map  (draw $ bulletSprite ss)     b  ++ 
  map  (draw $ asteroidSprite ss)   s  ++ 
  map  (draw $ tieFighterSprite ss) ts ++
  map  (draw $ laserSprite ss)      ls ++
  map  (draw $ destroyerSprite ss)  ds) 
viewPure (Pause s)                  = Pictures [viewPure s, color white (Translate (-200) 100 (text "Pause"))]

textHighscores :: [(String,Integer)] -> IO Picture
textHighscores list = do 
  let filetext = createText list 0
  return (Pictures filetext)
  where createText [] _ = []
        createText ((x,y):xs) i = Translate 0 i (scale 0.4 0.4 $ color white (text x)) : Translate 300 i ( scale 0.4 0.4 $ color white (text $ show y)) : createText xs (i - 50)

