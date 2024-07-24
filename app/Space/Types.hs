module Space.Types where

import Data.UUID (UUID)
import Linear (V3(..), V2)

class GameId a where
  gameId :: a -> UUID

data GameObject = forall a. (GameId a) => GameObject a

instance GameId GameObject where
  gameId (GameObject g) = gameId g

instance Eq GameObject where
  (==) (GameObject a) (GameObject b) = gameId a == gameId b

instance Ord GameObject where
  compare (GameObject a) (GameObject b) = compare (gameId a) (gameId b)

data PVA = PVA 
  { pvaPosition :: !(V3 Double)
  , pvaVelocity :: !(V3 Double)
  , pvaAcceleration :: !(V3 Double)
  }

data Planet = Planet
  { planetMass :: Double
  , planetRadius :: Double
  , planetId :: UUID
  } deriving (Eq, Ord, Show)

instance GameId Planet where
  gameId = planetId

data SpaceShip = SpaceShip
  { spaceShipMass :: Double
  , spaceShipLength :: Double
  , spaceShipID :: UUID
  } deriving (Eq, Ord, Show)

instance GameId SpaceShip where
  gameId = spaceShipID




