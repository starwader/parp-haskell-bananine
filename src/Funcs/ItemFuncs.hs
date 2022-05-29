module Funcs.ItemFuncs where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as M
import Defs.GameState
import Defs.Items
import Defs.Locations
import Funcs.IOFuncs

-- stworzenie nowego przedmiotu w lokalizacji
newItem :: Item -> GameStateIOT
newItem item = do
  gameState <- get
  let loc = currentLocation gameState
  case findLocationData loc $ locationsData gameState of
    Nothing -> lift $ printLines ["Taka lokalizacja nie istnieje", ""]
    Just locationData -> do
      let newLocationData = locationData {items = item : items locationData}
      modify
        ( const
            gameState
              { locationsData =
                  M.insert loc newLocationData $
                    locationsData gameState
              }
        )
      lift $ printLines ["", "    * możesz podnieść nowy przedmiot: " ++ item_name item ++ " *", ""]

-- dodanie nowego przedmiotu do ekwipunku
addItemToInventory :: Item -> GameStateIOT
addItemToInventory item = do
  gameState <- get
  lift $ printLines ["", "    * przedmiot " ++ item_name item ++ " został dodany do ekwipunku *", ""]
  modify (const gameState {inventory = item : inventory gameState})

-- usunięcie przedmiotu z ekwipunku
delItemFromInventory :: Item -> GameStateIOT
delItemFromInventory item = do
  gameState <- get
  lift $ printLines ["", "    * przedmiot " ++ item_name item ++ " został usunięty z ekwipunku *", ""]
  modify
    ( const
        gameState {inventory = filter (/= item) $ inventory gameState}
    )

-- sprawdzenie czy przedmiot jest w ekwipunku
itemInInventory :: Item -> GameState -> Bool
itemInInventory item gameState = elem item $ inventory gameState
