module Funcs.MoveFuncs where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import qualified Data.Map.Strict as M

import Defs.GameState
import Defs.Locations
import Defs.Npcs
import Defs.Inventory
import Defs.Tasks
import Defs.Interactions

import Funcs.Talk
import Funcs.Attack
import Funcs.IOFuncs

-- gos - go with GameStateIOT monad - proper implementation of go 
 
gos :: Direction -> GameStateIOT
gos d = do
    gameState <- get
    let oldLoc = currentLocation gameState
    let nextLocMaybe = go oldLoc d

    case nextLocMaybe of 
         Nothing -> lift $ printLines ["Nie możesz tędy iść", ""] 
         Just nextLoc -> do
             modify (\x -> gameState {currentLocation = nextLoc})
             printDescription
    
 
interacts :: Interaction -> Maybe String -> GameStateIOT
interacts interaction safeInteracted = do 
  case safeInteracted of
    Nothing -> lift $ printInteractionError interaction
    Just interacted -> do
      gameState <- get
      let loc = currentLocation gameState
      case findLocationData loc $ locationsData gameState of
        Nothing -> lift $ printLines ["Taka lokalizacja nie istnieje", ""]
        Just locationData -> do
          case interaction of 
            Talk -> talks interacted locationData 
            Drop -> drops interacted locationData 
            Attack -> attacks interacted locationData 
            Pickup -> pickups interacted locationData 


drops :: Item -> LocationData -> GameStateIOT
drops item locationData = do
  gameState <- get
  let inv = inventory gameState
  if elem item $ inv then do  
    let newLocationData = locationData {items = item:(items locationData)} 
    modify (\x -> gameState {
        inventory = filter (/=item) inv,
        locationsData = M.insert (currentLocation gameState) newLocationData $ locationsData gameState
        })
    lift $ printLines ["Upuściłeś ", item]
  else
    lift $ printInteractionError Drop

pickups :: Item -> LocationData -> GameStateIOT
pickups item locationData = do
  gameState <- get
  if elem item $ items locationData then do  
    let newLocationData = locationData {items = filter (/=item) $ items locationData} 
    modify (\x -> gameState {
        inventory = item:(inventory gameState),
        locationsData = M.insert (currentLocation gameState) newLocationData $ locationsData gameState
        })
    lift $ printLines ["Podniosłeś ", item]
  else
    lift $ printInteractionError Pickup

talks :: Npc -> LocationData -> GameStateIOT
talks npc locationData = do
  if elem npc $ npcs locationData then
    talk npc
  else
    lift $ printInteractionError Talk

attacks :: Npc -> LocationData -> GameStateIOT
attacks npc locationData = do
  if elem npc $ npcs locationData then
    attack npc
  else
    lift $ printInteractionError Attack
