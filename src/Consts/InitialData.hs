module Consts.InitialData where

import Consts.TextConstants
import qualified Data.Map.Strict as M
import Defs.Containers
import Defs.GameState
import Defs.Items (itemBanan, itemExcaliber, itemKluczDoFortu, itemZgniłyBanan, itemTajemniczyKlucz)
import Defs.Locations
import Defs.Npcs (npcAndrzej, npcBobo, npcGnom, npcKoko, npcPająk, npcUebe)
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
          { items = [itemBanan],
            containers =
              M.fromList
                [ ( "beczka",
                    emptyContainer
                      { store = [itemZgniłyBanan]
                      }
                  )
                ],
            desc = dzunglaDesc
          }
      ),
      ( "polanka",
        emptyLocationData
          { npcs = [npcKoko, npcBobo],
            desc = polankaDesc
          }
      ),
      ( "klasztor",
        emptyLocationData
          { npcs = [npcUebe],
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
                      { store = [itemExcaliber],
                        itemRequired = Just itemTajemniczyKlucz
                      }
                  )
                ]
          }
      ),
      ( "jpróby",
        emptyLocationData
          { npcs = [npcGnom],
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
          { npcs = [npcPająk],
            desc = jkobryDesc
          }
      ),
      ( "dziedziniec",
        emptyLocationData
          { npcs = [npcAndrzej],
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
