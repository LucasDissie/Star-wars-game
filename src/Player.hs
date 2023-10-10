module Player where
import Control.Monad.State.Lazy
import Model
import Bullet
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
movementSpeed = 300
size = 50 :: Float
bSpeed = 600 :: Float
shootingCooldown = 0.5


moveUp :: Float -> Float -> Float
moveUp a b | a + b > c = c
           | otherwise = a + b
             where c = (fromIntegral (screenHeight `div` 2) :: Float) - size


moveDown :: Float -> Float -> Float
moveDown a b | a - b < c = c
             | otherwise = a - b
             where c = (fromIntegral ((-screenHeight) `div` 2) ::Float) + size 

die :: Player -> State GameState ()
die p | lives p > 1 = do
  gstate <- get
  put (gstate{playerState = p{playerPosition = initialPlayerRect, cooldown = 0, lives = lives p - 1}, stones = [], bullets = [], blinks = [], tieFighters = [], destroyers = [], lasers = [], spawnTime = 1.5, playTime = playTime gstate - 20})
      | otherwise   = do
  gstate <- get
  put (Gameover (points p) "" False 5 (sprites gstate) (randomGen gstate))


instance Moveable Player where
  move time p@Player {playerInput = input, playerPosition = position}
    | 'w' `elem` input && 's' `elem` input = p
    | 'w' `elem` input = p {playerPosition = moveP moveUp} 
    | 's' `elem` input = p {playerPosition = moveP moveDown}  
    | otherwise = p
        where moveP f = position{y = f (y position) (movementSpeed * time)}




instance Shooter Player where
  shoot time p@Player{playerPosition = position, cooldown = cooldown}
    | 'S' `elem` playerInput p && cooldown < 0 = do
      addBullet (x position + 30) (y position - h position / 2 + 3) bSpeed
      addBullet (x position + 30) (y position + h position / 2 - 3) bSpeed
      modify (\gstate -> gstate {playerState = p {cooldown = shootingCooldown}})
    | otherwise = modify (\gstate -> gstate {playerState = p{cooldown = cooldown - time}} )
    

instance Drawable Player where
  draw sprite p@Player{playerPosition = hitbox@Rect{x = x, y = y, w = w, h = h}} = Translate x y (scaleToHitbox hitbox sprite)

instance Collider Player where
  getBox = playerPosition