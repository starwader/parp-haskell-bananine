module Funcs.EndingFuncs where

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import System.Exit

import Funcs.IOFuncs

import Defs.GameState


exit = exitWith(ExitSuccess)

die :: GameStateIOT
die = do
  lift $ printLines ["","Umarłeś"] 
  lift exit
