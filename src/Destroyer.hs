module Destroyer where
import Model
import Laser
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.State.Lazy
instance Moveable Destroyer where
  move t d@Destroyer {desPosition = pos, desSpeed = speed} =  
    d{desPosition = pos{x = newx (x pos)}}
    where newx x | x > 800 - w pos = x - t * speed
                 | otherwise       = x

instance Drawable Destroyer where
  draw sprite t@Destroyer{desPosition = hitbox@Rect{x = x, y = y}} = Translate x y (scaleToHitbox hitbox sprite)

instance Collider Destroyer where 
  getBox = desPosition

instance PointObject Destroyer where
  getPoints = desPoints

instance Updateable Destroyer where
  update t d@Destroyer{desTimer = timer} | timer < 0 = d{desTimer = 12}
                                         | otherwise = d{desTimer = timer - t}

updateDestroyers :: [Destroyer] -> State GameState()
updateDestroyers []     = return ()
updateDestroyers (x:xs) = do
  updateDestroyer x
  updateDestroyers xs

updateDestroyer :: Destroyer -> State GameState ()
updateDestroyer d@Destroyer{desTimer = timer, desPosition = pos} 
  | timer < 0 = addLaser (x pos) (y pos) (h pos)
  | otherwise = return ()

