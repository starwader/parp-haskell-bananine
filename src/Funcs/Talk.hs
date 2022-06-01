module Funcs.Talk where

import Consts.TextConstants
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Defs.GameState
import Defs.Items (itemKluczDoFortu, itemPortfelUebe, itemZepsutyKarabin, itemBanan, itemTajemniczyKlucz)
import Defs.Locations
import Defs.Npcs
import Defs.Skills (skillPotassiumSpell, skillTiuFiu)
import Defs.Tasks
import Funcs.EndingFuncs
import Funcs.IOFuncs
import Funcs.ItemFuncs
import Funcs.SkillFuncs
import Funcs.TaskFuncs
import Funcs.Teleport

-- rozmowa z npc
talk :: Npc -> GameStateIOT
talk (Npc NpcKoko _) = do
  gameState <- get
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
      if finishedTask taskKillBadGuys gameState
        then lift $ printLines kokoEndingPhrase
        else lift $ printLines kokoUebeQuestion
talk (Npc NpcBobo _) = do
  gameState <- get
  if itemInInventory itemBanan gameState
    then do
      lift $ printLines boboBananaQuestion
      ans : anss <- lift readCommand
      case ans of
        "tak" -> do
          delItemFromInventory itemBanan
          lift $ printLines boboRevealGun
          newItem itemZepsutyKarabin
        _ -> lift $ printLines boboSad
    else lift $ printLines boboDefault
talk (Npc NpcAndrzej _) = do
  lift $ printLines andrzejDefault
talk (Npc NpcPająk _) = do
  lift $ printLines pajakDefault
talk (Npc NpcGnom _) = do
  gameState <- get
  if activeTask taskTrial gameState
    then do
      lift $ printLines gnomQuestion
      ans : anss <- lift readCommand
      if ans `elem` ["człowiek", "czlowiek"]
        then do
          lift $ printLines $ concat gnomGoodAnswer:gnomTrialFinished
          modify (\x -> teleports gameState "klasztor")
          finishTask taskTrial
          printDescription
        else do
          lift $ printLines gnomBadAnswer
          if itemInInventory itemZepsutyKarabin gameState
          then do
            lift $ printLines gnomSeenGun
            delItemFromInventory itemZepsutyKarabin
            lift $ printLines gnomTrialFinished
            gameState <- get
            modify (\x -> teleports gameState "klasztor")
            finishTask taskTrial
            printDescription
          else die
    else lift $ printLines gnomDefault
talk (Npc NpcUebe _) = do
  gameState <- get
  let task_order = [taskTalkUebe, taskTrial, taskAttackUebe, taskFindWallet, taskKillBadGuys]
  let active_task = firstActiveTask task_order gameState
  let last_task = lastFinishedTask task_order gameState
  talkUebeWithTask last_task active_task

-- funkcja obsługująca interakcje zadań z Uebe
-- pierwszy parametr: ostatenie zakończone zadanie
-- drugi parametr: bieżące zadanie
talkUebeWithTask :: Maybe Task -> Maybe Task -> GameStateIOT
talkUebeWithTask Nothing Nothing = do
  lift $ printLines uebeNoKokoTask
talkUebeWithTask _ (Just (Task TaskTalkUebe _)) = do
  lift $ printLines uebeGivingProbaTask
  finishTask taskTalkUebe
  addTask taskTrial
talkUebeWithTask _ (Just (Task TaskTrial _)) = do
  lift $ printLines uebeProbaTask
talkUebeWithTask (Just (Task TaskTrial _)) Nothing = do
  lift $ printLines uebeAfterProba
  addTask taskAttackUebe
  addSkill skillPotassiumSpell
talkUebeWithTask _ (Just (Task TaskAttackUebe _)) = do
  lift $ printLines uebeAttackUebe
talkUebeWithTask _ (Just (Task TaskFindWallet _)) = do
  gameState <- get
  if itemInInventory itemPortfelUebe gameState
    then do
      lift $ printLines uebeLearningTiuFiu
      delItemFromInventory itemPortfelUebe
      addItemToInventory itemTajemniczyKlucz
      addSkill skillTiuFiu
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
