module Defs.Locations where

import qualified Data.Map.Strict as M
import Defs.Containers
import Defs.Items
import Defs.Npcs

-- lokalizacje

type Location = String

-- określenie możliwych ścieżek
go :: Location -> Direction -> Maybe Location
go "dżungla" North = Just "polanka"
go "polanka" South = Just "dżungla"
go "polanka" West = Just "klasztor"
go "klasztor" East = Just "polanka"
go "klasztor" North = Just "jpróby"
go "jpróby" South = Just "klasztor"
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

-- dane lokalizacji

-- znalezienie locationData w mapie locationData
findLocationData :: Location -> LocDataMap -> Maybe LocationData
findLocationData loc locDataMap = locDataMap M.!? loc

data LocationData = LocationData
  { npcs :: [Npc],
    items :: [Item],
    containers :: M.Map Container ContainerData,
    desc :: [String],
    additionalDesc :: [String]
  }

type LocDataMap = M.Map Location LocationData

-- strony świata

data Direction = North | South | West | East
