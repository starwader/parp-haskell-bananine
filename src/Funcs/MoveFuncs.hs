module Funcs.MoveFuncs where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class;

import Defs.GameState
import Defs.Locations
import Funcs.IOFuncs

-- gos - go with GameStateIOT monad - proper implementation of go 
 
gos :: Direction -> GameStateIOT
gos d = do
    gameState <- get
    let oldLoc = currentLocation(gameState)
    let nextLocMaybe = go oldLoc d
    case nextLocMaybe of 
         Nothing -> lift $ printLines ["Nie możesz tędy iść", ""] 
         Just nextLoc -> do
             modify (\x -> gameState {currentLocation = nextLoc})
             printDescription
    
 
teleports :: GameState -> Location -> GameState
teleports gameState loc = gameState { currentLocation = loc }
