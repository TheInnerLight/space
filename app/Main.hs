{-# LANGUAGE OverloadedStrings #-}

module Main where

import SDL                  (($=))
import qualified SDL as S
import qualified SDL.Font as SDLFont
import qualified SDL.Framerate as SF
import qualified SDL.Primitive as SP
import qualified SDL.Video.Renderer as SVR
import SDL.Vect             (V2(..), V3(..), V4(..), _x, _y, _z, Point(..))
import Space.App
import Space.Types
import Core (Has(has))
import Data.Foldable (traverse_, foldlM, foldrM)
import Space.Render (Render(render))
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.UUID.V4 as V4
import Data.Map ((!))
import qualified Data.Map as Map
import Control.Concurrent.STM (newTVarIO, atomically, STM, readTVarIO, readTVar, modifyTVar)
import Data.Time.Clock.System (getSystemTime, SystemTime (systemSeconds, systemNanoseconds))
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void)
import qualified Linear as L
import qualified Data.List as L

data Fonts = Fonts
  { fontsH2:: SDLFont.Font
  , fontsDefault :: SDLFont.Font
  }

processFps :: Int
processFps = 30

renderFPS :: Int
renderFPS = 60

black :: SP.Color
black = V4 0 0 0 255

loopFor :: S.Renderer -> Fonts-> SF.Manager -> IO ()
loopFor r fonts fpsm = do
  planetId1 <- V4.nextRandom
  moonId1 <- V4.nextRandom
  shipId1 <- V4.nextRandom
  moonPos <- newTVarIO $ Physics { physicsPosition = V3 384.4e6 0 0, physicsVelocity = V3 0 1.022e3 0, physicsMass = 7.3e22 }
  planetPos <- newTVarIO $ Physics { physicsPosition = V3 0 0 0, physicsVelocity = V3 0 0 0, physicsMass = 5.972e24 }
  shipPos <- newTVarIO $ Physics { physicsPosition = V3 1e6 0 0, physicsVelocity = V3 0 7.34e3 0, physicsMass = 10e9 }
  let initialState = 
        GameState 
          { gsPhysicsObjects = 
            Map.fromList
              [ (planetId1, PhysicsObject
                  { physObjPhysics = planetPos
                  , physObjEntity = PlanetEntity $ Planet { planetRadius = 6.371e6, planetId = planetId1 } 
                  })
              , (moonId1, PhysicsObject
                  { physObjPhysics = moonPos
                  , physObjEntity = PlanetEntity $ Planet { planetRadius = 1.7344e6, planetId = moonId1 } 
                  })
              , (shipId1, PhysicsObject
                  { physObjPhysics = shipPos
                  , physObjEntity = SpaceshipEntity $ SpaceShip { spaceShipLength = 10e3, spaceShipID = shipId1 } 
                  })
              ]
          }
  _ <- forkIO $ runAppM (processLoop processFps) initialState
  runAppM loop' initialState
  where
    loop' :: AppM ()
    loop' = do

      -- Clear the screen!
      S.rendererDrawColor r $= black
      S.clear r

      physObjs <- gsPhysicsObjects <$> has

      traverse_ (\po -> do

        phys <- liftIO $ readTVarIO $ physObjPhysics po

        case physObjEntity po of
          SpaceshipEntity se -> render r phys se
          PlanetEntity pe -> render r phys pe
        ) physObjs

      S.present r
      SF.delay_ fpsm
      loop'

processLoop :: Int -> AppM ()
processLoop desiredFps = do
  t1 <- liftIO getSystemTime
  let (n1 :: Integer) = fromIntegral (systemSeconds t1 * 1000000000) + fromIntegral (systemNanoseconds t1)
  update (1.0 / fromIntegral desiredFps)
  t2 <- liftIO getSystemTime
  let (n2 :: Integer) = fromIntegral (systemSeconds t2 * 1000000000) + fromIntegral (systemNanoseconds t2)
      microDiff = fromIntegral (n2-n1) `div` 1000
      eDiff = max 0 (1000000 `div` desiredFps - microDiff)
  liftIO $ threadDelay eDiff
  processLoop desiredFps



combinations :: [a] -> [(a, a)]
combinations [] = []
combinations [_] = []
combinations (x:xs) = fmap (x,) xs ++ combinations xs


-- combinations :: Int -> [a] -> [[a]]
-- combinations k ns = filter ((k==).L.length) $ L.subsequences ns

update :: Double -> AppM ()
update timestep = do

  physObjMap <- gsPhysicsObjects <$> has
  let physObjs = snd <$> Map.toList physObjMap

  let pairs = combinations physObjs

  -- liftIO $ print timestep
  liftIO $ print pairs

  gravitationalAccels <- 
    liftIO $ atomically $ foldrM (\(po1, po2) acc ->  do

      phys1 <- readTVar $ physObjPhysics po1
      phys2 <- readTVar $ physObjPhysics po2

      let v = gravity phys1 phys2
          v' = gravity phys2 phys1

      pure $ Map.insertWith (+) (gameId po1) v' $  Map.insertWith (+) (gameId po2) v acc
    ) Map.empty pairs

  liftIO $ print gravitationalAccels 

  void $ liftIO $ atomically $ traverse (\o -> do
        let gid = gameId o
        let v3 = gravitationalAccels ! gid
        modifyTVar (physObjPhysics o) (stepSVA timestep v3)
        ) physObjs


gravity :: Physics -> Physics -> V3 Double
gravity p1 p2 =
  let accel = (physicsMass p1 * 6.67430e-11)/ L.quadrance (physicsPosition p1 - physicsPosition p2)
  in  V3 accel accel accel * L.signorm (physicsPosition p1 - physicsPosition p2)

stepSVA :: Double -> V3 Double -> Physics -> Physics
stepSVA timestep acc pva =
  Physics
    { physicsPosition = physicsPosition pva + ((timestep * 3600 *) <$> vel)
    , physicsVelocity = vel
    , physicsMass = physicsMass pva
    }
  where vel = physicsVelocity pva + ((timestep * 3600 *) <$> acc)

thingy :: (V3 Double, V3 Double) -> (V3 Double, V3 Double) -> V3 Double
thingy (pStart, vStart) (pDest, vDest) =
  let 
      a = 20

      tv = L.norm (vDest - vStart) /a
      td = sqrt (2 * L.norm  (pDest - pStart) / a )

      ttotal = tv + td

      




      --ad = vDest - vStart / tf
      




  in V3 0 0 0


main :: IO ()
main = do
  S.initialize [S.InitVideo]
  SDLFont.initialize
  font <- SDLFont.load "Roboto-Regular.ttf" 18
  h2font <- SDLFont.load "Roboto-Regular.ttf" 32
  w <- S.createWindow "Space" S.defaultWindow { S.windowInitialSize = V2 screenWidth screenHeight }
  r <- S.createRenderer w (-1) S.defaultRenderer
  S.showWindow w

  let fonts = Fonts {fontsH2 = h2font, fontsDefault = font}
  
  SF.with renderFPS $ loopFor r fonts

  S.destroyWindow w
  S.quit
  where
    screenWidth = 1800
    screenHeight = 900
