module Space.Render where

import qualified SDL as S
import qualified SDL.Primitive as SP
import Space.Types
import SDL.Vect             (V2(..), V3(..), V4(..))
import Control.Lens ((^.))
import Linear (cross, normalize, zero)
import Linear.V3 (_xy, _x, _y)
import Space.App (AppM, Physics (..))
import Foreign.C (CInt)

green :: SP.Color
green = V4 9 100 9 255

grey :: SP.Color
grey = V4 200 200 200 255

blue :: SP.Color
blue = V4 8 74 172 255

class Render a where
  render :: S.Renderer -> a -> AppM ()

project2D :: V3 Double -> V2 CInt
project2D v3 =
  let scaled' = (v3 + V3 9e7 4.5e7 0) / 100000
  in V2 (floor $ scaled' ^. _x) (floor $ scaled' ^. _y) 

instance Render Planet where
  render r p = do
    pva <- getPVA (gameId p)
    let scaledSize = round $ planetRadius p / 100000
    SP.fillCircle r (project2D $ pvaPosition pva) (scaledSize) blue

instance Render SpaceShip where
  render r ss = do
    pva <- getPVA (gameId ss)
    let shipCentre = project2D $ pvaPosition pva
        direction = 
          if pvaAcceleration pva == zero then 
            V3 0 1 0 
          else
            normalize (pvaAcceleration pva)
        shipNormal = cross direction $ V3 0 0 1
        dir2D = V2 (floor $ 15 * direction ^. _x) (floor $ 15 * direction ^. _y)
        nor2D = V2 (floor $ 6 * shipNormal ^. _x) (floor $ 6 * shipNormal ^. _y)
    SP.fillTriangle r (shipCentre + dir2D) (shipCentre - dir2D + nor2D) (shipCentre - dir2D - nor2D) grey