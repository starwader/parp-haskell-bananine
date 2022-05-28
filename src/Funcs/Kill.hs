module Funcs.Kill where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict as M
import Defs.GameState
import Defs.Locations
import Defs.Npcs
import Funcs.IOFuncs

kill :: Npc -> Location -> GameStateIOT
kill npc loc = do
  gameState <- get
  let loc = currentLocation gameState
  case findLocationData loc $ locationsData gameState of
    Nothing -> lift $ printLines ["Taka lokalizacja nie istnieje", ""]
    Just locationData -> do
      let newLocationData = locationData {npcs = filter (/= npc) $ items locationData}
      modify
        ( const
            gameState
              { locationsData =
                  M.insert loc newLocationData $
                    locationsData gameState
              }
        )
