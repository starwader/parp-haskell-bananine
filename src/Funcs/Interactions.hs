module Funcs.Interactions where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as M
import Defs.Containers
import Defs.GameState
import Defs.Interactions
import Defs.Inventory
import Defs.Locations
import Defs.Npcs
import Defs.Tasks
import Funcs.Attack
import Funcs.IOFuncs
import Funcs.ItemFuncs
import Funcs.MoveFuncs
import Funcs.Talk
import Safe

-- funkcja wspólna dla interakcji z różnymi tworami (np przeciwnikami, skrzyniami, przedmiotami)
interacts :: Interaction -> [String] -> GameStateIOT
interacts interaction cmdsarg = do
  let safeInteracted = headMay cmdsarg
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
            Open -> containerInteracts interaction interacted (tail cmdsarg) locationData
            Put -> containerInteracts interaction interacted (tail cmdsarg) locationData
            Takeout -> containerInteracts interaction interacted (tail cmdsarg) locationData

-- odłożenie przedmiotu
puts :: Item -> Container -> LocationData -> GameStateIOT
puts item container locationData = do
  gameState <- get
  if itemInInventory item gameState
    then case containers locationData M.!? container of
      Nothing -> lift $ printInteractionError Open
      Just oldContainerData -> do
        lift $ printLines ["Odłożyłeś przedmiot " ++ item ++ " do " ++ container]
        let newContainerData = oldContainerData {store = item : store oldContainerData}
        let newLocationData = locationData {containers = M.insert container newContainerData (containers locationData)}
        modify
          ( const
              gameState
                { inventory = filter (/= item) (inventory gameState),
                  locationsData =
                    M.insert
                      (currentLocation gameState)
                      newLocationData
                      $ locationsData gameState
                }
          )
    else lift $ printInteractionError Drop

-- podniesienie przedmiotu z kontenera
takeouts :: Item -> Container -> LocationData -> GameStateIOT
takeouts item container locationData = do
  gameState <- get
  case containers locationData M.!? container of
    Nothing -> lift $ printInteractionError Open
    Just oldContainerData -> do
      if elem item $ store oldContainerData
        then do
          lift $ printLines ["Wyciągnąłeś przedmiot " ++ item ++ " z " ++ container]
          let newContainerData = oldContainerData {store = filter (/= item) (store oldContainerData)}
          let newLocationData = locationData {containers = M.insert container newContainerData (containers locationData)}
          modify
            ( const
                gameState
                  { inventory = item : inventory gameState,
                    locationsData =
                      M.insert
                        (currentLocation gameState)
                        newLocationData
                        $ locationsData gameState
                  }
            )
        else lift $ printLines ["W " ++ container ++ " nie ma przedmiotu " ++ item]

-- interakcja z kontenerem
containerInteracts :: Interaction -> String -> [String] -> LocationData -> GameStateIOT
containerInteracts interaction interacted cmds locationData = do
  gameState <- get
  let maybeContname = case length cmds of
        0 -> Just interacted
        2 -> headMay $ reverse cmds
        _ -> Nothing

  if null cmds && interaction /= Open
    then lift $ printLines ["Nieznana komenda - za mało argumentów"]
    else case maybeContname of
      Nothing -> lift $ printLines ["Nieznana komenda - nieprawidłowe argumenty"]
      Just contname -> do
        case containers locationData M.!? contname of
          Nothing -> lift $ printInteractionError Open
          Just cont -> do
            let canBeOpened = case itemRequired cont of
                  Nothing -> True
                  Just requirement -> elem requirement $ inventory gameState
            if canBeOpened
              then do
                case interaction of
                  Takeout -> takeouts interacted contname locationData
                  Put -> puts interacted contname locationData
                  Open -> opens contname locationData
                  _ -> lift $ printLines ["Nieprawidłowa interakcja"]
              else lift $ printLines ["Nie masz niczego co może otworzyć ten kontener"]

-- sprawdzenie zawartości kontenera
opens :: String -> LocationData -> GameStateIOT
opens containerName locationData = do
  case containers locationData M.!? containerName of
    Nothing -> lift $ printInteractionError Open
    Just cont -> printListWithDescFail ("Zawartość " ++ containerName ++ ":") ("Kontener " ++ containerName ++ " jest pusty") $ store cont

-- upuszczenie przedmiotu
drops :: Item -> LocationData -> GameStateIOT
drops item locationData = do
  gameState <- get
  let inv = inventory gameState
  if item `elem` inv
    then do
      let newLocationData = locationData {items = item : items locationData}
      modify
        ( const
            gameState
              { inventory = filter (/= item) inv,
                locationsData =
                  M.insert
                    (currentLocation gameState)
                    newLocationData
                    $ locationsData gameState
              }
        )
      lift $ printLines ["Upuściłeś ", item]
    else lift $ printInteractionError Drop

-- podniesienie przedmiotu
pickups :: Item -> LocationData -> GameStateIOT
pickups item locationData = do
  gameState <- get
  if elem item $ items locationData
    then do
      let newLocationData = locationData {items = filter (/= item) $ items locationData}
      modify
        ( const
        gameState
          {inventory = item : inventory gameState,
           locationsData = M.insert
                             (currentLocation gameState) newLocationData
                             $ locationsData gameState}
        )
      lift $ printLines ["Podniosłeś ", item]
    else lift $ printInteractionError Pickup

-- rozmowa z npc
talks :: Npc -> LocationData -> GameStateIOT
talks npc locationData = do
  if elem npc $ npcs locationData
    then talk npc
    else lift $ printInteractionError Talk

-- atak na npc
attacks :: Npc -> LocationData -> GameStateIOT
attacks npc locationData = do
  if elem npc $ npcs locationData
    then attack npc
    else lift $ printInteractionError Attack
