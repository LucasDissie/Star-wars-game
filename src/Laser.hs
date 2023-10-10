module Laser where
import Model
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.State.Lazy


instance Drawable Laser where
  draw sprite b@Laser{laserCollider = col@(Rect x y _ _)} = Translate x y (scaleToHitbox col sprite)

instance Updateable Laser where
  update t l@(Laser col@(Rect x _ w _) d) = Laser col{w = w + 500 * t, x = x - 250 * t} (d - t)

instance Collider Laser where
  getBox = laserCollider

addLaser :: Float -> Float -> Float -> State GameState ()
addLaser x y h = modify (\gstate -> gstate {lasers = Laser (Rect x y 0 h) 5 : lasers gstate})

checkLasers :: State GameState ()
checkLasers = modify (\gstate -> gstate{lasers = filter (\l -> duration l > 0) (lasers gstate)})