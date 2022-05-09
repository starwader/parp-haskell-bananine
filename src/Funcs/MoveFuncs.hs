module Funcs.MoveFuncs where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

import Defs.GameState
import Defs.Locations
import Defs.Tasks

import Funcs.IOFuncs
import Funcs.ItemFuncs
import Funcs.TaskFuncs

import Consts.TextConstants
 
gos :: Direction -> GameStateIOT
gos d = do
    gameState <- get
    let oldLoc = currentLocation gameState
    let nextLocMaybe = go oldLoc d

    case nextLocMaybe of 
        Nothing -> lift $ printLines ["Nie możesz tędy iść", ""] 
        Just "fort" -> do
            if not $ finishedTask taskKillBadGuys gameState then do
                if itemInInventory "klucz_do_fortu" gameState then do
                    modify (\x -> gameState {currentLocation = "fort"})
                    lift $ printLines killBadGuysText
                    finishTask taskKillBadGuys  
                else 
                    lift $ printLines ["Potrzebujesz klucza, aby tu wejść"]
            else do 
                modify (\x -> gameState {currentLocation = "fort"})
                printDescription
        Just nextLoc -> do
            modify (\x -> gameState {currentLocation = nextLoc})
            printDescription
    
