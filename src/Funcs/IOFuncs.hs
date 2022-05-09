module Funcs.IOFuncs where

import qualified Data.Map.Strict as M
import Control.Monad.Trans.State.Strict;
import Control.Monad.Trans.Class;
import Data.List.Split

import Defs.Locations
import Defs.GameState
import Defs.Interactions

import Consts.TextConstants

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)
 
printLocation :: Location -> IO ()
printLocation l = do
    let toPrint = "Znajdujesz sie w:"
    printLines [toPrint, l]
 
readCommand :: IO [String]
readCommand = do
    putStr "> "
    xs <- getLine
    let splitxs = splitOn " " xs 
    return splitxs

printListWithDesc :: String -> [String] -> GameStateIOT
printListWithDesc desc toPrintList = do
  if length toPrintList /= 0 then  
    lift $ printLines $ desc:(map ("- "++) toPrintList)
  else
    lift $ putStr ""  --todo

printListWithDescFail :: String -> String -> [String] -> GameStateIOT
printListWithDescFail desc failMsg toPrintList = do
  if length toPrintList /= 0 then  
    lift $ printLines $ desc:(map ("- "++) toPrintList)
  else
    lift $ printLines [failMsg]

printDescription :: GameStateIOT
printDescription = do
    gameState <- get
    let curLoc = currentLocation gameState
    let locationsDataMap = locationsData gameState
    let maybeLocationData = findLocationData curLoc locationsDataMap

    case maybeLocationData of
        Nothing -> lift $ printLines ["Taka lokalizacja nie istnieje", ""]
        Just locationData -> do
            let (locDesc, locAddDesc, its) = (desc locationData, additionalDesc locationData, items locationData)
            lift $ printLines $ desc locationData
            lift $ printLines $ additionalDesc locationData
            lift $ printLines [""]
            printListWithDesc "Możesz podnieść:" $ items locationData
            printListWithDesc "Dostępne postacie:" $ npcs locationData
            printListWithDesc "Kontenery:" $ M.keys $ containers locationData

printInteractionError :: Interaction -> IO()
printInteractionError Talk = printLines ["W tym miejscu nie ma takiej postaci", ""]
printInteractionError Attack = printLines ["W tym miejscu nie ma takiej postaci", ""]
printInteractionError Pickup = printLines ["W tym miejscu nie ma takiego przedmiotu", ""]
printInteractionError Drop = printLines ["Nie masz takiego przedmiotu w ekwipunku", ""]
printInteractionError Open = printLines ["Taki kontener nie istnieje", ""]
printInteractionError _ = printLines ["Błąd interakcji"]

