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

-- przejście do innego miejsca
gos :: Direction -> GameStateIOT
gos d = do
    gameState <- get
    let oldLoc = currentLocation gameState
    let nextLocMaybe = go oldLoc d
    tryChangeLocation nextLocMaybe

-- funkcja zmieniająca miejsce 
tryChangeLocation :: Maybe String -> GameStateIOT
tryChangeLocation Nothing = do
    lift $ printLines ["Nie możesz tędy iść", ""]
tryChangeLocation (Just "fort") = do
    gameState <- get
    if not $ finishedTask taskKillBadGuys gameState then do
        if itemInInventory "klucz_do_fortu" gameState then do
            modify (const gameState {currentLocation = "fort"})
            lift $ printLines killBadGuysText
            finishTask taskKillBadGuys
        else
            lift $ printLines ["Potrzebujesz klucza, aby tu wejść"]
    else do
        modify (const gameState {currentLocation = "fort"})
        printDescription
tryChangeLocation (Just nextLoc) = do
    gameState <- get
    modify (const gameState {currentLocation = nextLoc})
    printDescription
