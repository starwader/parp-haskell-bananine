module Funcs.TaskFuncs where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

import Defs.Tasks
import Defs.GameState
import Funcs.IOFuncs


addTask :: Task -> GameStateIOT
addTask task = do
  gameState <- get
  lift $ printLines ["","    Rozpoczęto zadanie: " ++ task]
  modify (\x ->gameState{tasks = task:(tasks gameState)})

finishTask :: Task -> GameStateIOT
finishTask task = do
  gameState <- get
  if elem task $ tasks gameState then do
    lift $ printLines ["","    Zadanie zakończone: " ++ task]
    modify (\x -> gameState {tasks = filter (/=task) $ tasks gameState})
   else
    lift $ printLines ["Zadanie ", task, " nie jest rozpoczęte"]

