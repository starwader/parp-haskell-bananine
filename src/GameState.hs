module GameState where

import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Strict;

import Locations

data GameState = GameState
     {
         currentLocation :: Location,
         locationsData :: M.Map Location LocationData
     }


type GameStateIOT = StateT GameState IO()
 
