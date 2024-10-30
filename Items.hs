module Items where

import Bin

data Item = N | Key Id | Trapdoor Id    deriving Show
type Id = Int

getItemId :: Item -> Id
getItemId (Key x) = x
getItemId (Trapdoor x) = x


getItemType :: Item -> String 
getItemType z = case z of 
  Key _ -> "Key"
  Trapdoor _ -> "Trapdoor"
  N -> ""

