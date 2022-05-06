module Adventure where

import qualified Data.Map.Strict as M
import Control.Monad.Trans.Class;
import Control.Monad.Trans.State.Strict;
import Control.Monad;

import Locations
import Inventory
import Npcs
import TextConstants
import IOFuncs
import GameState

-- "The Quest of Bananine"
-- 
--
-- Jakub Budrewicz
-- Marcel Jarosz
-- Przemysław Rozwałka
-- 
-- Projekt PARP 2022L


-- IO


    
gameLoop :: GameStateIOT
gameLoop = do
    s <- get
    cmd <- lift $ readCommand
    case cmd of
        "pomoc" -> do lift $ printLines instructionsText
                      gameLoop 

        "n" -> do gos North
                  gameLoop
        "s" -> do gos South
                  gameLoop
        "w" -> do gos West
                  gameLoop
        "e" -> do gos East
                  gameLoop
         
        "gdzie jestem" -> do lift $ printDescription s
                             gameLoop

        "koniec" -> return ()
        _ -> do lift $ printLines ["Nieznana komenda", ""]
                gameLoop 

gameMain = do
    printLines introductionText
    let polankaData = LocationData {
            npcs = ["Koko", "Bobo"],
            items = ["AK-74"]
        }
    let dzunglaData = LocationData {
            npcs = [],
            items = ["banan"]
        }
    let startGameState = GameState { 
        currentLocation = "dżungla", 
        locationsData = M.fromList [
            ("dżungla", dzunglaData), 
            ("polanka", polankaData)
        ]
    }
    printDescription startGameState
    printLines instructionsText
    runStateT gameLoop startGameState


gos :: Direction -> GameStateIOT  
gos d = do
    gameState <- get
    lift $ printDescription gameState
    let oldLoc = currentLocation(gameState)
    let nextLocMaybe = go oldLoc d
    let nextLoc = case nextLocMaybe of {
        Nothing -> oldLoc;
        Just x -> x;
    }
    modify (\x -> gameState {currentLocation = nextLoc})
    --gameState { currentLocation = nextLoc  }

teleports :: GameState -> Location -> GameState
teleports gameState loc = gameState { currentLocation = loc }





