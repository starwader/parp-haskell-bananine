module Adventure where

import qualified Data.Map.Strict as M
import Control.Monad.Trans.Class;
import Control.Monad.Trans.State.Strict;
import Control.Monad;

import Defs.Locations
import Defs.Inventory
import Defs.Npcs
import Defs.GameState

import Consts.TextConstants
import Consts.InitialData

import Funcs.IOFuncs
import Funcs.MoveFuncs

-- "The Quest of Bananine"
-- 
--
-- Jakub Budrewicz
-- Marcel Jarosz
-- Przemysław Rozwałka
-- 
-- Projekt PARP 2022L


    
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
         
        "gdzie jestem" -> do printDescription
                             gameLoop

        "koniec" -> return ()
        _ -> do lift $ printLines ["Nieznana komenda", ""]
                gameLoop 

gameMain :: IO()
gameMain = do
    printLines introductionText
    let startGameState = GameState { 
        currentLocation = "dżungla", 
        locationsData = initialLocationsData
    }
    runStateT printDescription startGameState
    printLines instructionsText
    void $ runStateT gameLoop startGameState

