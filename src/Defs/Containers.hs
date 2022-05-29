module Defs.Containers where

import Defs.Inventory
import Defs.Skills

-- kontenery (skrzynie, etc)

type Container = String

data ContainerData = ContainerData {store :: [Item], itemRequired :: Maybe Item}
