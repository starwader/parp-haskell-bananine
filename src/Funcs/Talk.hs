module Funcs.Talk where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

import Defs.GameState
import Defs.Locations
import Defs.Npcs

import Funcs.IOFuncs
import Funcs.TaskFuncs

import Consts.TextConstants

talk :: Npc -> GameStateIOT
talk "Koko" = do
  gameState <- get
  let fts = finishedTasks gameState
  let ts = tasks gameState
  if not $ elem "porozmawiaj z Uebe" fts then do
    lift $ printLines kokoWelcome 
    ans:anss <- lift $ readCommand 
    case ans of
      "tak" -> do
        lift $ printLines kokoUebeQuestionYes
        addTask "porozmawiaj z Uebe"
      _ -> lift $ printLines kokoUebeQuestionNo
  else if not $ elem "zabij kłusowników" fts then
    lift $ printLines kokoUebeQuestion
  else
    lift $ printLines kokoEndingPhrase

--talk "Bobo" = do
  

