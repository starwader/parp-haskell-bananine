module Adventure where

import Consts.InitialData
import Consts.TextConstants
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as M
import Defs.GameState
import Defs.Interactions
import Defs.Inventory
import Defs.Locations
import Defs.Npcs
import Funcs.IOFuncs
import Funcs.Interactions
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
  cmd : cmds <- lift readCommand
  case cmd of
    "pomoc" -> do
      lift $ printLines instructionsText
      gameLoop
    "n" -> do
      gos North
      gameLoop
    "s" -> do
      gos South
      gameLoop
    "w" -> do
      gos West
      gameLoop
    "e" -> do
      gos East
      gameLoop
    "rozmawiaj" -> do
      interacts Talk cmds
      gameLoop
    "atakuj" -> do
      interacts Attack cmds
      gameLoop
    "upuść" -> do
      interacts Drop cmds
      gameLoop
    "podnieś" -> do
      interacts Pickup cmds
      gameLoop
    "otwórz" -> do
      interacts Open cmds
      gameLoop
    "wyjmij" -> do
      interacts Takeout cmds
      gameLoop
    "umieść" -> do
      interacts Put cmds
      gameLoop
    "gdzie" -> do
      printDescription
      gameLoop
    "zadania" -> do
      printListWithDescFail "Aktywne zadania:" "Brak aktywnych zadań" $ tasks s
      printListWithDescFail "Zakończone zadania:" "Nie zakończyłeś jeszcze żadnego zadania" $ finishedTasks s
      gameLoop
    "ekwipunek" -> do
      printListWithDescFail "Ekwipunek:" "Twój ekwipunek jest pusty" $ inventory s
      gameLoop
    "umiejętności" -> do
      printListWithDescFail "Umiejętności:" "Nie masz żadnych umiejętności :(" $ skills s
      gameLoop
    "koniec" -> return ()
    _ -> do
      lift $ printLines ["Nieznana komenda", ""]
      gameLoop

gameMain :: IO ()
gameMain = do
  printLines introductionText
  runStateT printDescription initialGameState
  printLines instructionsText
  void $ runStateT gameLoop initialGameState
