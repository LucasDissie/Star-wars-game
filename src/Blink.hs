module Blink where
import Model
import Graphics.Gloss.Interface.IO.Game

instance Moveable Blink where
  move t b@Blink{blinkPosition = pos, blinkSpeed = speed} = 
    b{blinkPosition = pos{x = x pos - t * speed}}

instance Drawable Blink where
    draw sprite Blink{blinkPosition = hitbox@(Rect x y _ _)} = Translate x y (scaleToHitbox hitbox sprite)

instance Updateable Blink where
  update time b@Blink {blinkPosition = c, blinkSize = size, blinkTime = t} =  
    b{blinkPosition = c{w = newr, h = newr}, blinkTime = t + time}
      where newr = newRadius size t (blinkMult b)

newRadius :: Float -> Float -> Float -> Float
newRadius r t mult = r * (abs (cos t) * mult)

instance Collider Blink where 
  getBox = blinkPosition

instance PointObject Blink where
  getPoints = blinkPoints

