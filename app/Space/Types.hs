module Space.Types where

import Data.UUID (UUID)
import Linear (V3(..), V2)
import Control.Concurrent.STM (TVar)

class GameId a where
  gameId :: a -> UUID

data PhysicsEntity 
  = PlanetEntity Planet
  | SpaceshipEntity SpaceShip
  deriving (Eq, Ord, Show)

instance GameId PhysicsEntity where
  gameId (PlanetEntity p) = gameId p
  gameId (SpaceshipEntity s) = gameId s

data PhysicsObject = PhysicsObject
  { physObjPhysics :: TVar Physics
  , physObjEntity :: PhysicsEntity
  }

instance Show PhysicsObject where
  show po = show $ physObjEntity po


instance GameId PhysicsObject where
  gameId po = gameId (physObjEntity po)

data Physics = Physics 
  { physicsPosition :: !(V3 Double)
  , physicsVelocity :: !(V3 Double)
  , physicsMass :: !Double
  } deriving (Eq, Ord, Show)

data Planet = Planet
  { planetRadius :: Double
  , planetId :: UUID
  } deriving (Eq, Ord, Show)

instance GameId Planet where
  gameId = planetId

data SpaceShip = SpaceShip
  { spaceShipLength :: Double
  , spaceShipID :: UUID
  } deriving (Eq, Ord, Show)

instance GameId SpaceShip where
  gameId = spaceShipID




