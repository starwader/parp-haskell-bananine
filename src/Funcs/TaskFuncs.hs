module Funcs.TaskFuncs where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Defs.GameState
import Defs.Tasks
import Funcs.IOFuncs

addTask :: Task -> GameStateIOT
addTask task = do
  gameState <- get
  lift $ printLines ["", "    Rozpoczęto zadanie: " ++ task]
  modify (const gameState {tasks = task : tasks gameState})

finishTask :: Task -> GameStateIOT
finishTask task = do
  gameState <- get
  if elem task $ tasks gameState
    then do
      lift $ printLines ["", "    Zadanie zakończone: " ++ task]
      modify
        ( const
            gameState
              { finishedTasks = task : finishedTasks gameState,
                tasks = filter (/= task) $ tasks gameState
              }
        )
    else lift $ printLines ["Zadanie ", task, " nie jest rozpoczęte"]

finishedTask :: Task -> GameState -> Bool
finishedTask t gameState = elem t $ finishedTasks gameState

activeTask :: Task -> GameState -> Bool
activeTask t gameState = elem t $ tasks gameState

--finFirstNoSecondTask :: Task -> Task -> GameState -> Bool
--finFirstNoSecondTask t1 t2 gameState = finishedTask t1 gameState and noTaskYet t2 gameState

noTaskYet :: Task -> GameState -> Bool
noTaskYet t gameState = do
  let fts = finishedTasks gameState
  let ts = tasks gameState
  not $ elem t fts || elem t ts
