module TieFighter where
import Model
import Graphics.Gloss.Interface.IO.Game

instance Moveable TieFighter where
  move t tie@TieFighter{tiePosition = pos, tieSpeed = speed, tieDirection = dir} =  
    tie{tiePosition = pos{x = x pos - t * speed, y = y pos + dir * 6 * t}}

instance Drawable TieFighter where
  draw sprite t@TieFighter{tiePosition = p@Rect{x = x, y = y}} = Translate x y (scaleToHitbox p sprite)

instance Updateable TieFighter where
  update dir tie = case dir of 
    _ | dir > 10    -> tie{tieDirection = 10}
      | dir < (-10) -> tie{tieDirection = -10}
      | otherwise   -> tie{tieDirection = dir}

instance Collider TieFighter where 
  getBox = tiePosition

instance PointObject TieFighter where
  getPoints = tiePoints

getDifference :: Player -> TieFighter -> Float
getDifference p t = y (playerPosition p) - y (tiePosition t)