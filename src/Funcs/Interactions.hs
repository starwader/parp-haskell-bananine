module Funcs.Interactions where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as M
import Defs.Containers
import Defs.GameState
import Defs.Interactions
import Defs.Items
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
puts :: String -> Container -> LocationData -> GameStateIOT
puts itemName container locationData = do
  gameState <- get
  case getItemByName (inventory gameState) itemName of
    Nothing -> lift $ printInteractionError Put
    Just item -> case containers locationData M.!? container of
      Nothing -> lift $ printInteractionError Open
      Just oldContainerData -> do
        lift $ printLines ["Odłożyłeś przedmiot " ++ item_name item ++ " do " ++ container]
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

-- podniesienie przedmiotu z kontenera
takeouts :: String -> Container -> LocationData -> GameStateIOT
takeouts itemName container locationData = do
  gameState <- get
  case containers locationData M.!? container of
    Nothing -> lift $ printInteractionError Open
    Just oldContainerData -> case getItemByName (store oldContainerData) itemName of
      Nothing -> lift $ printLines ["W " ++ container ++ " nie ma przedmiotu " ++ itemName]
      Just item -> do
        lift $ printLines ["Wyciągnąłeś przedmiot " ++ item_name item ++ " z " ++ container]
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
    Just cont ->
      printListWithDescFail
        ("Zawartość " ++ containerName ++ ":")
        ("Kontener " ++ containerName ++ " jest pusty")
        $ map item_name $ store cont

-- upuszczenie przedmiotu
drops :: String -> LocationData -> GameStateIOT
drops itemName locationData = do
  gameState <- get
  let inv = inventory gameState
  case getItemByName inv itemName of
    Nothing -> lift $ printInteractionError Drop
    Just item -> do
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
      lift $ printLines ["Upuściłeś ", item_name item]

-- podniesienie przedmiotu
pickups :: String -> LocationData -> GameStateIOT
pickups itemName locationData = do
  gameState <- get
  case getItemByName (items locationData) itemName of
    Nothing -> lift $ printInteractionError Pickup
    Just item -> do
      let newLocationData = locationData {items = filter (/= item) $ items locationData}
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
      lift $ printLines ["Podniosłeś " ++ item_name item]

-- rozmowa z npc
talks :: String -> LocationData -> GameStateIOT
talks npcName locationData = do
  case getNpcByName (npcs locationData) npcName of
    Just foundNpc -> talk foundNpc
    Nothing -> lift $ printInteractionError Talk

-- atak na npc
attacks :: String -> LocationData -> GameStateIOT
attacks npcName locationData = do
  case getNpcByName (npcs locationData) npcName of
    Just foundNpc -> attack foundNpc
    Nothing -> lift $ printInteractionError Attack
