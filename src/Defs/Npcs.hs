module Defs.Npcs where

import Data.List

data NpcType
  = NpcUebe
  | NpcPająk
  | NpcAndrzej
  | NpcKoko
  | NpcBobo
  | NpcGnom
  deriving (Eq)

type NpcName = String

data Npc = Npc
  { npc_type :: NpcType,
    npc_name :: NpcName
  }
  deriving (Eq)

-- wszystkie postacie
npcAndrzej = Npc NpcAndrzej "Andrzej"
npcPająk = Npc NpcPająk "Pająk"
npcUebe = Npc NpcUebe "Uebe"
npcKoko = Npc NpcKoko "Koko"
npcBobo = Npc NpcBobo "Bobo"
npcGnom = Npc NpcGnom "Gnom"

-- funkcja pomocnicza wyszukująca postać po nazwie
-- pierwszy parametr: lista postaci
-- drugi parametr: nazwa wyszukiwanej postaci
getNpcByName :: [Npc] -> String -> Maybe Npc
getNpcByName npcs name = find (\n -> npc_name n == name) npcs