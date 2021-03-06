module Inventory where

import qualified Data.Map.Strict as M

data ItemType
  = Clothing
  | HighTech
  | Book
  deriving (Show, Eq, Ord, Enum, Bounded)

data ItemDesc
  = ItemDesc
      String -- nom
      Double -- coût
      ItemType -- type
  deriving (Show, Eq)

-- un catalogue associe les identifiants de descriptions (la clef) avec
-- leurs descriptions (la valeur)
type Catalog = M.Map Int ItemDesc

-- un inventaire associe les identifiants de descriptions (la clef) avec
-- leurs quantités en stock (la valeur)
type Inventory = M.Map Int Int

sampleCatalog :: Catalog
sampleCatalog = M.fromList
  [ (1, ItemDesc "Neuromancer" 7.85 Book)
  , (2, ItemDesc "Rashômon" 2 Book)
  , (3, ItemDesc "Huawei P20 Lite" 247 HighTech)
  , (4, ItemDesc "Xiaomi Redmi Note 5" 197 HighTech)
  , (5, ItemDesc "WearAll - Femmes Tricoté Longue Manche Renne Noël 3D Chandail Dames Flocon De Neige Cavalier" 15 Clothing)
  ]

sampleInventory :: Inventory
sampleInventory = M.fromList
  [ (1, 12)
  , (2, 4)
  , (3, 8)
  , (4, 1)
  , (5, 43234)
  ]

data Search
    = Or [Search]
    | And [Search]
    | Not Search
    | Name String
    | Cost Ordering Double
    | TypeIs ItemType
    deriving (Show, Eq)

getName :: ItemDesc -> String
getName i = case i of
            ItemDesc name _ _ -> name

getCost :: ItemDesc -> Double
getCost i = case i of
              ItemDesc _ cost _ -> cost

getType :: ItemDesc -> ItemType
getType i = case i of
              ItemDesc _ _ itype -> itype

check :: Search -> ItemDesc -> Bool
check search desc =
    case search of
      Name nom -> nom == getName desc


-- indice, regarder la page de description du type Map
-- https://www.stackage.org/haddock/nightly-2018-07-08/containers-0.5.11.0/Data-Map-Strict.html


-- Utiliser la fonction M.filter
filterCatalog :: Search -> Catalog -> Catalog
filterCatalog = undefined

-- Utiliser la fonction M.keys
catalogItems :: Catalog -> [Int]
catalogItems = undefined

-- utiliser M.filter, et la fonction `elem` (dans Data.List)
filterInventory :: [Int] -> Inventory -> Inventory
filterInventory = undefined

-- Utiliser les fonctions précédentes, et la fonction `toList`
search :: Search -> Catalog -> Inventory -> [(ItemDesc, Int)]
search = undefined
