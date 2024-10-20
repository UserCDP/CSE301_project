module Items where

import Bin

data Item = N | Key Id | Trapdoor Id    deriving Show
type Id = Int

getItemId :: Item -> Id
getItemId (Key x) = x
getItemId (Trapdoor x) = x


getItemTypeAtNode :: BinZip Item -> String 
getItemTypeAtNode z = case val z of 
  Key _ -> "Key"
  Trapdoor _ -> "Trapdoor"
  N -> "Nothing"


