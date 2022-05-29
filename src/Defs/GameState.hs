module Defs.GameState where

import Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as M
import Defs.Inventory
import Defs.Locations
import Defs.Skills
import Defs.Tasks

-- główny stan gry

data GameState = GameState
  { tasks :: [Task],
    finishedTasks :: [Task],
    inventory :: Inventory,
    skills :: [Skill],
    currentLocation :: Location,
    locationsData :: M.Map Location LocationData
  }


-- transformata StateT (tworzy monadę z GameState i IO)
type GameStateIOT = StateT GameState IO ()
