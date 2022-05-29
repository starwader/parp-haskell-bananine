module Funcs.EndingFuncs where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Defs.GameState
import Funcs.IOFuncs
import System.Exit

-- funkcje kończące grę

exit = exitSuccess

win :: GameStateIOT
win = do
  lift $ printLines ["", "Zwycięstwo!!!"]
  lift exit

die :: GameStateIOT
die = do
  lift $ printLines ["", "Umarłeś"]
  lift exit
