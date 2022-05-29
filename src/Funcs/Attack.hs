module Funcs.Attack where

import Consts.TextConstants
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as M
import Defs.GameState
import Defs.Items (itemExcaliber, itemKluczDoFortu, itemPortfelUebe)
import Defs.Locations
import Defs.Npcs
import Defs.Skills (skillPotassiumSpell)
import Defs.Tasks
import Funcs.EndingFuncs
import Funcs.IOFuncs
import Funcs.ItemFuncs
import Funcs.Kill
import Funcs.SkillFuncs
import Funcs.TaskFuncs

-- logika walki z przeciwnikami

attack :: Npc -> GameStateIOT
attack (Npc NpcKoko _) = do
  lift $ printLines kokoAttack
  die
attack (Npc NpcBobo _) = do
  lift $ printLines boboAttack
  die
attack (Npc NpcAndrzej _) = do
  gameState <- get
  if elem itemExcaliber $ inventory gameState
    then do
      lift $ printLines andrzejKill
      let loc = currentLocation gameState
      case findLocationData loc $ locationsData gameState of
        Nothing -> lift $ printLines ["Taka lokalizacja nie istnieje", ""]
        Just locationData -> do
          kill npcAndrzej loc
          newItem itemKluczDoFortu
    else do
      lift $ printLines andrzejAttackDefault
      die
attack (Npc NpcPająk _) = do
  gameState <- get
  if elem skillPotassiumSpell $ skills gameState
    then do
      lift $ printLines pajakKill
      let loc = currentLocation gameState
      case findLocationData loc $ locationsData gameState of
        Nothing -> lift $ printLines ["Taka lokalizacja nie istnieje", ""]
        Just locationData -> do
          kill npcPająk loc
          newItem itemPortfelUebe
    else do
      lift $ printLines pajakAttackDefault
      die
attack (Npc NpcUebe _) = do
  gameState <- get
  if activeTask taskAttackUebe gameState
    then do
      lift $ printLines uebeAttackTask
      finishTask taskAttackUebe
      addTask taskFindWallet
    else do
      lift $ printLines uebeAttackDefault
      die
attack _ = do
  lift $ printLines unkillableDefault
