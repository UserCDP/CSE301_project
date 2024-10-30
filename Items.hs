module Items where

import Bin
import System.IO

data Item = N | Key Id | Debris | Chest Bool Id [Item] | Shovel | Map Id | OxigenTank Id   deriving (Show, Read, Eq)
type Id = Int


getItemType :: Item -> String 
getItemType z = case z of 
  Key _ -> "Key"
  Chest _ _ _ -> "Chest"
  Debris -> "Debris"
  Shovel -> "Shovel"
  Map _ -> "Map"
  OxigenTank _ -> "Oxigen"
  N -> ""


getItemId :: Item -> Id
getItemId x = 
  case x of
    Key id -> id
    Chest _ id _ -> id
    Map id -> id
    OxigenTank id -> id

checkListContainsItemType :: String -> [Item] -> Bool
checkListContainsItemType x l = x `elem` (map (\z -> getItemType z) l)

