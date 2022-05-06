module Defs.GameState where

import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Strict

import Defs.Locations
import Defs.Inventory

data GameState = GameState
     {
         inventory :: Inventory,
         currentLocation :: Location,
         locationsData :: M.Map Location LocationData
     }


type GameStateIOT = StateT GameState IO()
 
