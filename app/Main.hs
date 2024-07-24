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
import Data.Foldable (traverse_)
import Space.Render (Render(render))
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.UUID.V4 as V4
import qualified Data.Map as Map
import Control.Concurrent.STM (newTVarIO, atomically)
import Data.Time.Clock.System (getSystemTime, SystemTime (systemSeconds, systemNanoseconds))
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void)

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
  shipId1 <- V4.nextRandom
  planetPos <- newTVarIO $ PVA { pvaPosition = V3 0 0 0, pvaAcceleration = V3 0 0 0, pvaVelocity = V3 0 0 0 }
  shipPos <- newTVarIO $ PVA { pvaPosition = V3 1e7 0 0, pvaAcceleration = V3 0 0 0, pvaVelocity = V3 0 0 0 }
  let initialState = 
        GameState 
          { gsPlanets = 
            [ Planet { planetMass = 5.972e24, planetRadius = 6.371e6, planetId = planetId1 } 
            ]
          , gsShips = 
            [ SpaceShip { spaceShipMass = 10e9, spaceShipLength = 10e3, spaceShipID = shipId1 } 
            ]
          , gsPositions = Map.fromList
            [ (planetId1, planetPos)
            , (shipId1,   shipPos)
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

      planets <- gsPlanets <$> has

      traverse_ (render r) planets

      ships <- gsShips <$> has

      traverse_ (render r) ships

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

update :: Double -> AppM ()
update timestep = do
  ships <- gsShips <$> has
  stuff <- traverse (\s -> modifyPVA (gameId s) (stepSVA timestep) ) ships
  let stuff' = sequence stuff
  void $ liftIO $ atomically $ stuff'

gravity :: Planet -> SpaceShip -> AppM ()
gravity p ss = do

  --modifyPVA (gameId ss) 

  pure ()

stepSVA :: Double -> PVA -> PVA
stepSVA timestep pva =
  PVA
    { pvaPosition = pvaPosition pva + ((timestep *) <$> pvaVelocity pva)
    , pvaVelocity = pvaVelocity pva + ((timestep *) <$> pvaAcceleration pva)
    , pvaAcceleration = pvaAcceleration pva
    }

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
