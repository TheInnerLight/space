module Space.App where

import Core
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))
import Control.Monad.IO.Class
import Space.Types
import Data.Map (Map, (!))
import Data.UUID (UUID)
import Control.Concurrent.STM (TVar, readTVarIO, writeTVar, STM, modifyTVar)

data GameState = GameState 
  { gsPlanets :: [Planet]
  , gsShips :: [SpaceShip]
  , gsPositions :: Map UUID (TVar PVA)
  }

newtype AppM a = 
  AppM {unAppM :: ReaderT GameState IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

runAppM :: AppM a -> GameState -> IO a
runAppM = runReaderT . unAppM

instance Has AppM GameState where
  has = AppM ask

class Physics m where
  getPVA :: UUID -> m PVA
  setPVA :: UUID -> PVA -> m (STM ())
  modifyPVA :: UUID -> (PVA -> PVA) -> m (STM ())

instance Physics AppM where
  getPVA uuid = do 
    pvaMap <- gsPositions <$> has
    liftIO . readTVarIO $ pvaMap ! uuid
  setPVA uuid pva = do
    pvaMap <- gsPositions <$> has
    pure $ writeTVar (pvaMap ! uuid) pva
  modifyPVA uuid f = do
    pvaMap <- gsPositions <$> has
    pure $ modifyTVar (pvaMap ! uuid) f








