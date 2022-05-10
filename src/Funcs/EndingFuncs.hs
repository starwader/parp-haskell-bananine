module Funcs.EndingFuncs where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import System.Exit

import Funcs.IOFuncs

import Defs.GameState


exit = exitWith(ExitSuccess)

win :: GameStateIOT
win = do
  lift $ printLines ["", "Zwycięstwo!!!"]
  lift exit

die :: GameStateIOT
die = do
  lift $ printLines ["","Umarłeś"] 
  lift exit
