module Funcs.Attack where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import qualified Data.Map.Strict as M

import Defs.GameState
import Defs.Locations
import Defs.Npcs
import Defs.Tasks

import Funcs.IOFuncs
import Funcs.TaskFuncs
import Funcs.EndingFuncs
import Funcs.ItemFuncs

import Consts.TextConstants

attack :: Npc -> GameStateIOT
attack "Koko" = do
  lift $ printLines kokoAttack
  die
attack "Bobo" = do
  lift $ printLines boboAttack
  die
attack "Andrzej" = do
  lift $ printLines andrzejAttackDefault
  die
attack "Pająk" = do
  gameState <- get
  if activeTask taskFindWallet gameState then do
    lift $ printLines pajakKill
    let loc = currentLocation gameState
    case findLocationData loc $ locationsData gameState of
      Nothing -> lift $ printLines ["Taka lokalizacja nie istnieje", ""]
      Just locationData -> do
        let newLocationData = locationData
        newItem "Portfel"
  else do
    lift $ printLines pajakAttackDefault
    die
attack "Uebe" = do
  gameState <- get
  if activeTask taskAttackUebe gameState then do
    lift $ printLines uebeAttackTask 
    finishTask taskAttackUebe
    addTask taskFindWallet
  else do 
    lift $ printLines uebeAttackDefault
    die
attack _ = do
  lift $ printLines unkillableDefault

