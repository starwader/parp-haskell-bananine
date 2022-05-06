module Funcs.IOFuncs where

import Control.Monad.Trans.State.Strict;
import Control.Monad.Trans.Class;

import Defs.Locations
import Defs.GameState

import Consts.TextConstants

printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)
 
printLocation :: Location -> IO ()
printLocation l = do
    let toPrint = "Znajdujesz sie w:"
    printLines [toPrint, l]
 
readCommand :: IO String
readCommand = do
    putStr "> "
    xs <- getLine
    return xs

printDescription :: GameStateIOT
printDescription = do
    gameState <- get
    let curLoc = currentLocation gameState
    let locationsDataMap = locationsData gameState
    let maybeLocationData = findLocationData curLoc locationsDataMap

    case maybeLocationData of
        Nothing -> lift $ printLines ["Taka lokalizacja nie istnieje", ""]
        Just locationData -> do
            let (locDesc, locAddDesc) = (desc locationData, additionalDesc locationData)
            lift $ printLines locDesc
            lift $ printLines locAddDesc


