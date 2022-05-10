module Funcs.Talk where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

import Defs.GameState
import Defs.Locations
import Defs.Npcs
import Defs.Tasks

import Funcs.IOFuncs
import Funcs.SkillFuncs
import Funcs.TaskFuncs
import Funcs.EndingFuncs
import Funcs.Teleport
import Funcs.ItemFuncs

import Consts.TextConstants


talk :: Npc -> GameStateIOT
talk "Koko" = do
  gameState <- get
  let fts = finishedTasks gameState
  let ts = tasks gameState
  if noTaskYet taskTalkUebe gameState then do
    lift $ printLines kokoWelcome 
    ans:anss <- lift $ readCommand 
    case ans of
      "tak" -> do
        lift $ printLines kokoUebeQuestionYes
        addTask taskTalkUebe
      _ -> lift $ printLines kokoUebeQuestionNo
  else if not $ elem taskKillBadGuys fts then
    lift $ printLines kokoUebeQuestion
  else
    lift $ printLines kokoEndingPhrase
talk "Bobo" = do
  lift $ printLines boboDefault
talk "Andrzej" = do
  lift $ printLines andrzejDefault
talk "Pająk" = do
  lift $ printLines pajakDefault
talk "Gnom" = do
  gameState <- get
  if activeTask taskTrial gameState then do
    lift $ printLines gnomQuestion 
    ans:anss <- lift $ readCommand 
    case ans of
      "człowiek" -> do
        lift $ printLines gnomGoodAnswer
        modify (\x -> teleports gameState "klasztor")
        finishTask taskTrial
        printDescription
      _ -> do 
        lift $ printLines gnomBadAnswer
        die
  else 
    lift $ printLines gnomDefault
talk "Uebe" = do
  gameState <- get
  if noTaskYet taskTalkUebe gameState then do
    lift $ printLines uebeNoKokoTask
  else if activeTask taskTalkUebe gameState then do  
    lift $ printLines uebeGivingProbaTask
    addTask taskTrial
    finishTask taskTalkUebe
  else if activeTask taskTrial gameState then -- proba jeszcze nieukonczona 
    lift $ printLines uebeProbaTask
  else if finishedTask taskTrial gameState then do
    if noTaskYet taskAttackUebe gameState then do
      lift $ printLines uebeAfterProba 
      addTask taskAttackUebe
      addSkill "zaklęcie Potassium"
    else if activeTask taskAttackUebe gameState then 
      lift $ printLines uebeAttackUebe 
    else if activeTask taskFindWallet gameState then do
      if elem "portfel" $ inventory gameState then do
        lift $ printLines uebeLearningTiuFiu
        delItemFromInventory "portfel"
        addItemToInventory "klucz"
        addSkill "Tiu Fiu"
        finishTask taskFindWallet
        addTask taskKillBadGuys
      else
        lift $ printLines uebeWalletTask
    else if activeTask taskKillBadGuys gameState then 
      lift $ printLines uebeLearnedTiuFiu
    else if finishedTask taskKillBadGuys gameState then do
      lift $ printLines uebeEnding
      win
    else 
      lift $ printLines ["błąd 2"]
  else 
    lift $ printLines ["błąd 1"]
talk _ = do
  lift $ printLines ["Ta postać nie ma zaimplementowanej rozmowy"]


--talk "Bobo" = do
  

