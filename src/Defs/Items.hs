module Defs.Items where

import Data.List

data ItemType
  = ItemBanan
  | ItemZgniłyBanan
  | ItemExcaliber
  | ItemKluczDoFortu
  | ItemTajemniczyKlucz
  | ItemPortfelUebe
  | ItemZepsutyKarabin
  deriving (Eq)

type ItemName = String

data Item = Item
  { item_type :: ItemType,
    item_name :: ItemName
  }
  deriving (Eq)

type Inventory = [Item]

-- wszystkie przedmioty w grze
itemBanan = Item ItemBanan "banan"
itemZgniłyBanan = Item ItemBanan "zgniły_banan"
itemExcaliber = Item ItemExcaliber "Excaliber"
itemTajemniczyKlucz = Item ItemTajemniczyKlucz "tajemniczy_klucz"
itemKluczDoFortu = Item ItemKluczDoFortu "klucz_do_fortu"
itemPortfelUebe = Item ItemPortfelUebe "portfel"
itemZepsutyKarabin = Item ItemZepsutyKarabin "zepsuty_karabin"

-- funkcja pomocnicza do wyszukiwania przedmiotów po nazwie
-- pierwszy parametr: lista przedmiotów
-- drugi parametr: nazwa wyszukiwanego przedmiotu
getItemByName :: [Item] -> String -> Maybe Item
getItemByName inventory itemName =
  find (\i -> item_name i == itemName) inventory