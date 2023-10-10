module Bullet where
import Model
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.State.Lazy
instance Moveable Bullet where 
  move t b@Bullet{bulletPosition = pos, bulletSpeed = speed}
    = b {bulletPosition = pos {x = x pos + t * speed}}

instance Drawable Bullet where
  draw sprite b@Bullet{bulletPosition = position@(Rect x y _ _)} = Translate x y (scaleToHitbox position sprite)


instance Collider Bullet where
  getBox = bulletPosition

addBullet :: Float -> Float -> Float -> State GameState ()
addBullet x y speed = modify (\gstate -> gstate{bullets =  Bullet {bulletPosition = Rect x y 15 5, bulletSpeed = speed} : bullets gstate})