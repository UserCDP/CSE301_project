module Items where

import Bin
import System.IO

data Item = N | Key Id | Debris | Chest Bool Id [Item] | Shovel | Map Id | OxygenTank Id   deriving (Show, Read, Eq)


type Id = Int


getItemType :: Item -> String 
getItemType z = case z of 
  Key _ -> "Key"
  Chest _ _ _ -> "Chest"
  Debris -> "Debris"
  Shovel -> "Shovel"
  Map _ -> "Map"
  OxygenTank _ -> "Oxygen"
  N -> ""


getItemId :: Item -> Id
getItemId x = 
  case x of
    Key id -> id
    Chest _ id _ -> id
    Map id -> id
    OxygenTank id -> id

checkListContainsItemType :: String -> [Item] -> Bool
checkListContainsItemType x l = x `elem` (map (\z -> getItemType z) l)

showItemList :: [Item] -> String
showItemList (x:[]) = show x
showItemList (x : xs) = show x ++ ", " ++ showItemList xs 
