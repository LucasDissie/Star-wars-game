module GameLoop where
import Control.Monad.State.Lazy
import Model
import Player
import Bullet
import Laser
import Stone
import Blink
import TieFighter 
import Destroyer
import Randomness
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe
import System.Random


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@Playing{} = do 
    let newState = execState (updateState secs) gstate
    return newState

step secs gstate@Pause{} = return gstate
step secs gstate@Gameover{written = w, score = p} = do 
  scores <- highscores
  if not w && check scores
    then return gstate
    else do
      let newtimer = timer gstate - secs
      if newtimer < 0
        then return $ initialMenuState (sprites gstate) (randomGen gstate) -- return to menu
        else return gstate{timer = newtimer}
  where check ss = p > snd (last ss)
step secs gstate@Menu{} = return gstate



updateState :: Float -> State GameState ()
updateState secs = do
  spawnEnemies secs
  modify (\gstate -> gstate{playTime = playTime gstate + secs})
  updateMovement secs
  updateUpdates secs
  updateShooting secs
  updateCollisions 


-- Movement for all objects
updateMovement :: Float -> State GameState ()
updateMovement secs = modify (\gstate -> gstate {playerState = move secs (playerState gstate)
                                                , bullets = map (move secs) (bullets gstate)
                                                , stones = map (move secs) (stones gstate)
                                                , blinks = map (move secs) (blinks gstate)
                                                , tieFighters = map (move secs) (tieFighters gstate)
                                                , destroyers = map (move secs) (destroyers gstate)})



-- Variable updates for all objects
updateUpdates :: Float -> State GameState ()
updateUpdates secs = do 
  modify (\gstate -> gstate {stones = map (update secs) (stones gstate)
                            , blinks = map (update secs) (blinks gstate)
                            , tieFighters = map (\t -> update (getDifference (playerState gstate) t) t) (tieFighters gstate)
                            , destroyers = map (update secs) (destroyers gstate)
                            , lasers = map (update secs) (lasers gstate)})
  thisstate <- get
  updateDestroyers (destroyers thisstate)


-- Shooting updates for all objects
updateShooting :: Float -> State GameState ()
updateShooting secs = do
  thisstate <- get
  let p = playerState thisstate
  shoot secs p

-- Collisions for all objects
updateCollisions :: State GameState ()
updateCollisions = do
  collisions stones setStones
  collisions blinks setBlinks
  collisions tieFighters setTieFighters
  collisions destroyers setDestroyers
  screenCollisions bullets setBullets 
  screenCollisions stones setStones
  screenCollisions blinks setBlinks
  screenCollisions tieFighters setTieFighters
  screenCollisions destroyers setDestroyers
  checkLasers
  playerCollisions stones
  playerCollisions blinks
  playerCollisions tieFighters
  playerCollisions lasers

playerCollisions :: Collider b => (GameState -> [b]) -> State GameState ()
playerCollisions getbs = do
  thisstate <- get
  case thisstate of
    Playing{} -> do
      let bs = getbs thisstate
      let player = playerState thisstate
      when (not (null bs) && any (collision player) bs) $ die player
    _         -> return ()

screenCollisions :: Collider a => (GameState -> [a]) -> ([a] -> State GameState ()) -> State GameState ()
screenCollisions getxs f = do 
  thisstate <- get
  let xs = getxs thisstate
  f [x | x <- xs, screenCollision x]



collisions :: (Collider b, PointObject b) => (GameState -> [b]) -> ([b] -> State GameState ()) -> State GameState ()
collisions getbs tobs = do
  thisstate <- get
  let as = bullets thisstate
  let bs = getbs thisstate
  let player = playerState thisstate
  put (thisstate{playerState = player{points = points player + newPoints as bs}})
  setBullets (newAs as bs)
  tobs (newBs as bs)
  where newPoints as bs = sum (map getPoints (deletedBs as bs)) 
        deletedAs as bs = [a | a <- as, any (collision a) bs]
        deletedBs as bs = [b | b <- bs, any (collision b) as]
        newAs as bs  = [a | a <- as, not $ any (collision a) bs]
        newBs as bs = [b | b <- bs, not $ any (collision b) as]


-- Spawning
spawnEnemies :: Float -> State GameState ()
spawnEnemies time = do 
  stime <- getSpawnTime
  if stime < 0
    then do 
      difficulty <- getDifficulty
      getRandomEnemies difficulty
      newTime <- getRandomFloat
      maxTime <- getMaxSpawnTime
      setSpawnTime (newTime * maxTime)
    else updateTime time 
    where
      spawn 0 = return ()
      spawn i = do

        spawn (i-1)
      getMaxSpawnTime = state  (\gstate -> (100 / (playTime gstate + 50), gstate))      :: State GameState Float -- make spawntime smaller the further you get
      getSpawnTime    = state  (\gstate -> (spawnTime gstate, gstate))                  :: State GameState Float
      setSpawnTime t  = modify (\gstate -> gstate{spawnTime = t})                       :: State GameState ()
      updateTime time = modify (\gstate -> gstate{spawnTime = spawnTime gstate - time}) :: State GameState ()
      getDifficulty   = state  (\gstate -> (round(0.03 * playTime gstate + 1),gstate))  :: State GameState Int -- increase difficulty every ~30 seconds
      



      
