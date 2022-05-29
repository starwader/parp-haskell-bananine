module Funcs.TaskFuncs where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Defs.GameState
import Defs.Tasks
import Funcs.IOFuncs
import Data.Maybe (listToMaybe)
import Data.List (find)

-- dodanie zadania
addTask :: Task -> GameStateIOT
addTask task = do
  gameState <- get
  lift $ printLines ["", "    Rozpoczęto zadanie: " ++ task_desc task]
  modify (const gameState {tasks = task : tasks gameState})

-- zakonczenie zadania
finishTask :: Task -> GameStateIOT
finishTask task = do
  gameState <- get
  if elem task $ tasks gameState
    then do
      lift $ printLines ["", "    Zadanie zakończone: " ++ task_desc task]
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
  let fts = finishedTasks gameState
  let ts = tasks gameState
  not $ elem t fts || elem t ts

-- 
firstActiveTask :: [Task] -> GameState -> Maybe Task
firstActiveTask taskOrder gameState = find evalTask taskOrder
  where
    evalTask :: Task -> Bool
    evalTask currentTask = activeTask currentTask gameState

-- 
lastFinishedTask :: [Task] -> GameState -> Maybe Task
lastFinishedTask [] _ = Nothing
lastFinishedTask taskOrder gameState = find (`finishedTask` gameState) (reverse taskOrder)
