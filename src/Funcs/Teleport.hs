module Funcs.Teleport where

import Defs.GameState
import Defs.Locations

teleports :: GameState -> Location -> GameState
teleports gameState loc = gameState {currentLocation = loc}
