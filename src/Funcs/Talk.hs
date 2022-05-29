module Funcs.Talk where

import Consts.TextConstants
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Defs.GameState
import Defs.Locations
import Defs.Npcs
import Defs.Tasks
import Funcs.EndingFuncs
import Funcs.IOFuncs
import Funcs.ItemFuncs
import Funcs.SkillFuncs
import Funcs.TaskFuncs
import Funcs.Teleport

-- rozmowa z npc
talk :: Npc -> GameStateIOT
talk "Koko" = do
  gameState <- get
  let fts = finishedTasks gameState
  let ts = tasks gameState
  if noTaskYet taskTalkUebe gameState
    then do
      lift $ printLines kokoWelcome
      ans : anss <- lift readCommand
      case ans of
        "tak" -> do
          lift $ printLines kokoUebeQuestionYes
          addTask taskTalkUebe
        _ -> lift $ printLines kokoUebeQuestionNo
    else
      if taskKillBadGuys `notElem` fts
        then lift $ printLines kokoUebeQuestion
        else lift $ printLines kokoEndingPhrase
talk "Bobo" = do
  lift $ printLines boboDefault
talk "Andrzej" = do
  lift $ printLines andrzejDefault
talk "Pająk" = do
  lift $ printLines pajakDefault
talk "Gnom" = do
  gameState <- get
  if activeTask taskTrial gameState
    then do
      lift $ printLines gnomQuestion
      ans : anss <- lift readCommand
      case ans of
        "człowiek" -> do
          lift $ printLines gnomGoodAnswer
          modify (\x -> teleports gameState "klasztor")
          finishTask taskTrial
          printDescription
        _ -> do
          lift $ printLines gnomBadAnswer
          die
    else lift $ printLines gnomDefault

talk "Uebe" = do
  gameState <- get
  let task_order = [taskTalkUebe, taskTrial, taskAttackUebe, taskFindWallet, taskKillBadGuys]
  let active_task = firstActiveTask task_order gameState
  let last_task = lastFinishedTask task_order gameState
  talkUebeWithTask last_task active_task

talk _ = do
  lift $ printLines ["Ta postać nie ma zaimplementowanej rozmowy"]

-- 
-- pierwszy parametr: ostatenie zakończone zadanie
-- second parameter: current active task
talkUebeWithTask :: Maybe Task -> Maybe Task -> GameStateIOT
talkUebeWithTask Nothing Nothing = do
  lift $ printLines uebeNoKokoTask

talkUebeWithTask _ (Just (Task TaskTalkUebe _)) = do
  lift $ printLines uebeGivingProbaTask
  addTask taskTrial
  finishTask taskTalkUebe

talkUebeWithTask _ (Just (Task TaskTrial _)) = do
  lift $ printLines uebeProbaTask

talkUebeWithTask (Just (Task TaskTrial _)) Nothing = do
  lift $ printLines uebeAfterProba
  addTask taskAttackUebe
  addSkill "zaklęcie Potassium"

talkUebeWithTask _ (Just (Task TaskAttackUebe _)) = do
  lift $ printLines uebeAttackUebe

talkUebeWithTask _ (Just (Task TaskFindWallet _)) = do
  gameState <- get
  if elem "portfel" $ inventory gameState
    then do
      lift $ printLines uebeLearningTiuFiu
      delItemFromInventory "portfel"
      addItemToInventory "klucz"
      addSkill "Tiu Fiu"
      finishTask taskFindWallet
      addTask taskKillBadGuys
    else lift $ printLines uebeWalletTask

talkUebeWithTask _ (Just (Task TaskKillBadGuys _)) = do 
  lift $ printLines uebeLearnedTiuFiu

talkUebeWithTask (Just (Task TaskKillBadGuys _)) _ = do
  gameState <- get
  lift $ printLines uebeEnding
  win

talkUebeWithTask _ _ = do 
  gameState <- get
  lift $ printLines ["Czego chcesz?"]
