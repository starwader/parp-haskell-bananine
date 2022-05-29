module Funcs.Teleport where

import Defs.GameState
import Defs.Locations

-- funkcja arbitralnie zmieniająca lokalizację
teleports :: GameState -> Location -> GameState
teleports gameState loc = gameState {currentLocation = loc}
