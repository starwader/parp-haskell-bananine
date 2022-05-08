module Funcs.Kill where

import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

import Defs.GameState
import Defs.Locations
import Defs.Npcs

import Funcs.IOFuncs



kill :: Npc -> Location -> GameStateIOT
kill npc loc = do
  gameState <- get
  let loc = currentLocation gameState
  case findLocationData loc $ locationsData gameState of
    Nothing -> lift $ printLines ["Taka lokalizacja nie istnieje", ""]
    Just locationData -> do
      let newLocationData = locationData {npcs = filter (/=npc) $ items locationData}
      modify (\x -> gameState {
        locationsData = M.insert loc newLocationData $ locationsData gameState
      })