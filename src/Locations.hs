module Locations where

import qualified Data.Map.Strict as M

import Npcs
import Inventory
import GameState

-- locations
   
type Location = String
   
go :: Location -> Direction -> Maybe Location
go "dżungla" North = Just "polanka"
go "polanka" South = Just "dżungla"
go "polanka" West = Just "klasztor"
go "klasztor" East = Just "polanka"
go "klasztor" North = Just "jpróby"
go "polanka" East = Just "rozwidlenie"
go "rozwidlenie" West = Just "polanka"
go "rozwidlenie" North = Just "kamieniolom"
go "kamieniolom" South = Just "rozwidlenie"
go "rozwidlenie" South = Just "jkobry"
go "jkobry" North = Just "rozwidlenie"
go "rozwidlenie" East = Just "dziedziniec"
go "dziedziniec" West = Just "rozwidlenie"
go "dziedziniec" East = Just "fort"
go "fort" West = Just "dziedziniec"
go a b = Nothing
   

-- location data
   
findLocationData :: Location -> LocDataMap -> Maybe LocationData
findLocationData loc locDataMap = locDataMap M.!? loc -- case filter ((== loc) . locationTemp) locationDatas of
    --[] -> Nothing
    --(location:rest) -> Just location
   
data LocationData = LocationData 
    {
        npcs :: [Npc],
        items :: [Item]
    }

type LocDataMap = M.Map Location LocationData

-- directions

data Direction = North | South | West | East

-- gos - go with GameStateIOT monad - proper implementation of go 

gos :: Direction -> GameStateIOT
gos d = do
    gameState <- get
    lift $ printDescription gameState
    let oldLoc = currentLocation(gameState)
    let nextLocMaybe = go oldLoc d
    let nextLoc = case nextLocMaybe of {
        Nothing -> oldLoc;
        Just x -> x;
    }
    modify (\x -> gameState {currentLocation = nextLoc})
    --gameState { currentLocation = nextLoc  }

-- teleport - todo monadic

teleports :: GameState -> Location -> GameState
teleports gameState loc = gameState { currentLocation = loc }
