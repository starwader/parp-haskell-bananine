module Defs.Containers where

import Defs.Inventory
import Defs.Skills

type Container = String

data ContainerData = ContainerData { store :: [Item], itemRequired :: Maybe Item }


