module Stone where
import Model
import Graphics.Gloss.Interface.IO.Game

instance Moveable Stone where
  move t s@Stone{stonePosition = pos, stoneSpeed = speed} = 
    s{stonePosition = pos{x = x pos - t * speed}}

instance Drawable Stone where
    draw sprite s@Stone{stonePosition = position@(Rect x y _ _), stoneRotation = rotation} = Translate x y (Rotate rotation (scaleToHitbox position sprite))

instance Updateable Stone where
  update t s@Stone{stoneRotation = r}= s{stoneRotation = r + 20 * t} 

instance Collider Stone where 
  getBox = stonePosition

instance PointObject Stone where
  getPoints = stonePoints




