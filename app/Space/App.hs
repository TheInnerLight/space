module Space.App where

import Core
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader (ask))
import Control.Monad.IO.Class
import Space.Types
import Data.Map (Map, (!))
import Data.UUID (UUID)
import Control.Concurrent.STM (TVar, readTVarIO, writeTVar, STM, modifyTVar)

data GameState = GameState 
  { gsPhysicsObjects :: Map UUID PhysicsObject
  }

newtype AppM a = 
  AppM {unAppM :: ReaderT GameState IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

runAppM :: AppM a -> GameState -> IO a
runAppM = runReaderT . unAppM

instance Has AppM GameState where
  has = AppM ask










