module Defs.Locations where

import qualified Data.Map.Strict as M

import Defs.Npcs
import Defs.Inventory

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
go "rozwidlenie" North = Just "kamieniołom"
go "kamieniołom" South = Just "rozwidlenie"
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
        items :: [Item],
        desc :: [String],
        additionalDesc :: [String]
    }

type LocDataMap = M.Map Location LocationData

-- directions

data Direction = North | South | West | East

