module Consts.InitialData where

import qualified Data.Map.Strict as M

import Defs.Locations

import Consts.TextConstants

emptyLocationData = LocationData {
    npcs = [],
    items = [],
    desc = [],
    additionalDesc = []
}

initialLocationsData = M.fromList [
    ("dżungla", emptyLocationData {
            items = ["banan"],
            desc = dzunglaDesc
        }
    ),("polanka", emptyLocationData {
            npcs = ["Koko", "Bobo"],
            desc = polankaDesc
        }
    ),("klasztor", emptyLocationData {
            desc = klasztorDesc
        }
    ),("kamieniołom", emptyLocationData {
            desc = kamieniolomDesc
        }
    ),("jpróby", emptyLocationData {
            npcs = ["Gnom"],
            desc = jprobyDesc
        }
    ),("rozwidlenie", emptyLocationData {
            desc = rozwidlenieDesc
        }
    ),("jkobry", emptyLocationData {
            npcs = ["Pająk"],
            desc = jkobryDesc
        }
    ),("dziedziniec", emptyLocationData {
            npcs = ["Andrzej"],
            desc = dziedziniecDesc
        }
    ),("fort", emptyLocationData {
            desc = fortDesc
        }
    )
    ]


