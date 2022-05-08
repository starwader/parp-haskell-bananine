module Funcs.ItemFuncs where

import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

import Defs.Inventory
import Defs.Locations
import Defs.GameState

import Funcs.IOFuncs

newItem :: Item -> GameStateIOT
newItem item = do
  gameState <- get
  let loc = currentLocation gameState
  case findLocationData loc $ locationsData gameState of
    Nothing -> lift $ printLines ["Taka lokalizacja nie istnieje", ""]
    Just locationData -> do
      let newLocationData = locationData {items = item:(items locationData)}
      modify (\x -> gameState {
        locationsData = M.insert loc newLocationData $ locationsData gameState
      })
      lift $ printLines ["Możesz podnieść nowy przedmiot: ", item]
