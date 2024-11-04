module ItemsLogic where

import Items
import GameData
import Messages
import Scripts
import Data.List

pickUpKey :: Game -> IO Game
pickUpKey game =  
    case getCurrentItem game of
        Key id -> do
            displayString "new key" 
            return game {pos = removeItemAtNode (pos game), inventory = (Key id : inventory game)}
        _ -> do
            displayString "no key here"
            return game


useKey :: Item -> Game -> IO Game
useKey (Key id) game =
     case getCurrentItem game of
        Chest False x _ -> 
            let hasKey = (Key id) `elem` (inventory game) in
            let goodKey = id == x in
            case (hasKey, goodKey) of
                (True, True) -> do
                    displayString "unlocked chest"
                    return game { pos = unlockChestAtNode (pos game)}

                (True, False) -> do
                    displayString "wrong key"
                    return game

                (False, _) -> do
                    displayString "dont have key"
                    return game
                
        Chest True _ _ -> do 
            displayString "chest already open"
            return game

        _ -> do
            displayString "nothing to open"
            return game

checkChest :: Game -> IO Game
checkChest game =  
    case getCurrentItem game of
        Chest True _ [] -> do
            displayString "empty chest"
            return game
   
        Chest True _ objs  -> do
            displayString "emptying chest"
            return game {pos = emptyChestAtNode (pos game), inventory = inventory game ++ objs}
                
        Chest False _ _-> do
            displayString "locked chest"
            return game
        _ -> do
            displayString "no chest here"
            return game

useShovel :: Game -> IO Game
useShovel game =
    let shovelBool = checkYouHaveItem "Shovel" game in
    let debrisBool = getCurrentItemType game == "Debris" in
    case (shovelBool, debrisBool) of
        (True, True) -> do
            displayString "clear way"
            return game{pos = removeItemAtNode (pos game)}
        
        (True, False) -> do
            displayString "shovel is useless"
            return game
        (False, _ ) -> do
            displayString "no shovel"
            return game

useOxygenTank :: Item -> Game -> IO Game
useOxygenTank (OxygenTank ox ) game = do
  if OxygenTank ox `elem` (inventory game) then do
    displayString "used oxygen tank"
    return game{inventory = delete (OxygenTank ox) (inventory game), oxygen = (oxygen game) + 100}
  else do
    displayString "dont have oxygen tank"
    return game
