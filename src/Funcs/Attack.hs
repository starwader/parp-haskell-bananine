module Funcs.Attack where

import Consts.TextConstants
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as M
import Defs.GameState
import Defs.Locations
import Defs.Npcs
import Defs.Tasks
import Funcs.EndingFuncs
import Funcs.IOFuncs
import Funcs.ItemFuncs
import Funcs.Kill
import Funcs.SkillFuncs
import Funcs.TaskFuncs

attack :: Npc -> GameStateIOT
attack "Koko" = do
  lift $ printLines kokoAttack
  die
attack "Bobo" = do
  lift $ printLines boboAttack
  die
attack "Andrzej" = do
  gameState <- get
  if elem "Excaliber" $ inventory gameState
    then do
      lift $ printLines andrzejKill
      let loc = currentLocation gameState
      case findLocationData loc $ locationsData gameState of
        Nothing -> lift $ printLines ["Taka lokalizacja nie istnieje", ""]
        Just locationData -> do
          kill "Andrzej" loc
          newItem "klucz_do_fortu"
    else do
      lift $ printLines andrzejAttackDefault
      die
attack "Pająk" = do
  gameState <- get
  if elem "zaklęcie Potassium" $ skills gameState
    then do
      lift $ printLines pajakKill
      let loc = currentLocation gameState
      case findLocationData loc $ locationsData gameState of
        Nothing -> lift $ printLines ["Taka lokalizacja nie istnieje", ""]
        Just locationData -> do
          kill "Pająk" loc
          newItem "portfel"
    else do
      lift $ printLines pajakAttackDefault
      die
attack "Uebe" = do
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
