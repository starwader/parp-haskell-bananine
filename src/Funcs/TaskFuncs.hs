module Funcs.TaskFuncs where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.List (find)
import Data.Maybe (listToMaybe)
import Defs.GameState
import Defs.Tasks
import Funcs.IOFuncs

-- dodanie zadania
addTask :: Task -> GameStateIOT
addTask task = do
  gameState <- get
  lift $ printLines ["", "    * zadanie rozpoczęte: " ++ task_desc task ++ " *", ""]
  modify (const gameState {tasks = task : tasks gameState})

-- zakończenie zadania
finishTask :: Task -> GameStateIOT
finishTask task = do
  gameState <- get
  if elem task $ tasks gameState
    then do
      lift $ printLines ["", "    * zadanie zakończone: " ++ task_desc task ++ " *", ""]
      modify
        ( const
            gameState
              { finishedTasks = task : finishedTasks gameState,
                tasks = filter (/= task) $ tasks gameState
              }
        )
    else lift $ printLines ["Zadanie ", task_desc task, " nie jest rozpoczęte"]

-- sprawdzenie czy zadanie jest zakonczone
finishedTask :: Task -> GameState -> Bool
finishedTask t gameState = elem t $ finishedTasks gameState

-- sprawdzenie czy zadanie jest aktywne
activeTask :: Task -> GameState -> Bool
activeTask t gameState = elem t $ tasks gameState

-- sprawdzenie czy zadanie jeszcze nie istnieje
noTaskYet :: Task -> GameState -> Bool
noTaskYet t gameState = do
  not $ finishedTask t gameState || activeTask t gameState

-- zwraca pierwsze aktywne zadanie wg. podanej kolejności
-- pierwszy parametr: kolejność wykonywania zadań
-- drugi parametr: stan gry
firstActiveTask :: [Task] -> GameState -> Maybe Task
firstActiveTask taskOrder gameState = find evalTask taskOrder
  where
    evalTask :: Task -> Bool
    evalTask currentTask = activeTask currentTask gameState

-- zwraca ostatnie zakończone zadanie wg. podanej kolejności
-- pierwszy parametr: kolejność wykonywania zadań
-- drugi parametr: stan gry
lastFinishedTask :: [Task] -> GameState -> Maybe Task
lastFinishedTask [] _ = Nothing
lastFinishedTask taskOrder gameState =
   find (`finishedTask` gameState) (reverse taskOrder)
