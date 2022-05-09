module Funcs.SkillFuncs where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

import Defs.Skills
import Defs.GameState

import Funcs.IOFuncs

addSkill :: Skill -> GameStateIOT
addSkill skill = do
  gameState <- get
  lift $ printLines ["","    Nowa umiejętność: " ++ skill]
  modify (\x ->gameState{skills = skill:(skills gameState)})
