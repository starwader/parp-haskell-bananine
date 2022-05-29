module Consts.InitialData where

import Consts.TextConstants
import qualified Data.Map.Strict as M
import Defs.Containers
import Defs.GameState
import Defs.Locations
import Defs.Tasks

-- pomocnicze, puste LocationData i Container
emptyLocationData =
  LocationData
    { containers = M.empty,
      npcs = [],
      items = [],
      desc = [],
      additionalDesc = []
    }

emptyContainer =
  ContainerData
    { store = [],
      itemRequired = Nothing
    }

-- dane początkowe dla GameState
initialLocationsData =
  M.fromList
    [ ( "dżungla",
        emptyLocationData
          { items = ["banan"],
            containers =
              M.fromList
                [ ( "beczka",
                    emptyContainer
                      { store = ["zgniły_banan"]
                      }
                  )
                ],
            desc = dzunglaDesc
          }
      ),
      ( "polanka",
        emptyLocationData
          { npcs = ["Koko", "Bobo"],
            desc = polankaDesc
          }
      ),
      ( "klasztor",
        emptyLocationData
          { npcs = ["Uebe"],
            desc = klasztorDesc
          }
      ),
      ( "kamieniołom",
        emptyLocationData
          { desc = kamieniolomDesc,
            containers =
              M.fromList
                [ ( "skrzynia",
                    emptyContainer
                      { store = ["Excaliber"],
                        itemRequired = Just "klucz"
                      }
                  )
                ]
          }
      ),
      ( "jpróby",
        emptyLocationData
          { npcs = ["Gnom"],
            desc = jprobyDesc
          }
      ),
      ( "rozwidlenie",
        emptyLocationData
          { desc = rozwidlenieDesc
          }
      ),
      ( "jkobry",
        emptyLocationData
          { npcs = ["Pająk"],
            desc = jkobryDesc
          }
      ),
      ( "dziedziniec",
        emptyLocationData
          { npcs = ["Andrzej"],
            desc = dziedziniecDesc
          }
      ),
      ( "fort",
        emptyLocationData
          { desc = fortDesc
          }
      )
    ]

initialGameState =
  GameState
    { tasks = [],
      finishedTasks = [],
      inventory = [],
      currentLocation = "dżungla",
      skills = [],
      locationsData = initialLocationsData
    }
