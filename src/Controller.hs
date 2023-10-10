-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Graphics.Gloss.Interface.IO.Game
import System.Random
import System.Exit
import Control.DeepSeq

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate@Gameover {} = inputKeyGameover e gstate
input e gstate@Menu {}     = inputKeyMenu e gstate
input e gstate             = return (inputKey e gstate)


inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char a) Down _ _) gstate@Playing{ playerState = p@Player{playerInput = x}}
  = gstate {playerState = p {playerInput = a : x} }
inputKey (EventKey (Char a) Up _ _) gstate@Playing{ playerState = p@Player{playerInput = x}}
  = gstate {playerState = p {playerInput = filter (/= a) x} }
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate@Playing {}
  = Pause gstate
inputKey (EventKey (SpecialKey KeyEsc) Down _ _) gstate@(Pause state)
  = state
inputKey (EventKey (SpecialKey KeySpace) Down _ _) gstate@Playing{ playerState = p@Player{playerInput = x}}
  = gstate {playerState = p {playerInput = 'S' : x} }
inputKey (EventKey (SpecialKey KeySpace) Up _ _) gstate@Playing{ playerState = p@Player{playerInput = x}}
  = gstate {playerState = p {playerInput = filter (/= 'S') x} }
inputKey _ gstate = gstate -- Otherwise keep the same

inputKeyGameover :: Event -> GameState -> IO GameState
inputKeyGameover (EventKey (Char a) Down _ _) gstate@Gameover{ name = name} 
  | length name < 8 = return gstate {name = name ++ [a]}
  | otherwise       = return gstate 
inputKeyGameover (EventKey (SpecialKey KeyDelete) Down _ _) gstate@Gameover {name = name}
  | null name = return gstate
  | otherwise = return gstate {name = init name}
inputKeyGameover (EventKey (SpecialKey KeyEnter) Down _ _) gstate@Gameover {name = name}
  | null name = return gstate -- keep it the same, because there cant be empty names
  | otherwise = do
    scores <- highscores
    rnf scores `seq` writeFile "files/highscores.txt" (createText (take 5 (insert (name,score gstate) scores))) -- werkt niet
    return gstate{written = True}
  where insert (n,s) ((n',s'):xs) | s > s'    = (n,s) : (n',s') : xs
                                  | otherwise = (n',s') : insert (n,s) xs
        createText [] = ""
        createText ((n,s):xs) = n ++ " " ++ show s ++ "\n" ++ createText xs 
inputKeyGameover _  gstate = return gstate

inputKeyMenu :: Event -> GameState -> IO GameState
inputKeyMenu (EventMotion pos) gstate = return gstate {mouse = pos}
inputKeyMenu (EventKey (MouseButton LeftButton) Up _ _) gstate@Menu {mouse = (x,y)}
   | x > (-133) && x < 85 && y < 107 && y > 60       = return $ initialPlayingState (sprites gstate) (randomGen gstate)
   | x > (-206) && x < 192 && y < 4 && y > (-45)     = return (Gameover (-100) "" False 5 (sprites gstate) (randomGen gstate))
   | x > (-91) && x < 57 && y < (-101) && y > (-148) = exitSuccess
   | otherwise = return gstate
inputKeyMenu _ gstate = return gstate


-- 667 393  885 440
-- 594 496    992 545
-- 709 601    857 648