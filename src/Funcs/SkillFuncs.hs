module Funcs.SkillFuncs where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Defs.GameState
import Defs.Skills
import Funcs.IOFuncs

-- dodanie umiejętności
addSkill :: Skill -> GameStateIOT
addSkill skill = do
  gameState <- get
  lift $ printLines ["", "    Nowa umiejętność: " ++ skill]
  modify (const gameState {skills = skill : skills gameState})
