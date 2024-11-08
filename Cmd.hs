module Cmd where

import GameData
import Items
import Bin
import Scripts
import Messages
import ItemsLogic



--- Translates Name of the command into an actual function
executeCommand :: Maybe Cmd -> (Game -> IO Game)
executeCommand (Just Go_Left) = goLeft
executeCommand (Just Go_Right) = goRight
executeCommand (Just Go_Down) = goDown
executeCommand (Just Go_Back) = goBack
executeCommand (Just Go_Up) = goUp
executeCommand (Just (Use x) ) = useItem x
executeCommand (Just PickUp ) = getItem 
executeCommand (Just Check_Inventory) = checkInventory
executeCommand (Just Quit) = quitGame
executeCommand (Just Show_Oxygen) = showOxygen
executeCommand (Just Help) = helpGamer
executeCommand Nothing = retakeCommand


--- Implement movement commands
goLeft :: Game -> IO Game
goLeft game = 
  if getCurrentItemType (pos game) == "Debris" then do
    displayString "blocked way"
    return game
  else
    case pos game of
    (c, T _ Empty _ _) -> do
                               displayString "out of bounds"
                               return game
    (c,T x t1 t2 t3) -> return game { pos = incrementNodeVisits (T0 x c t2 t3,t1), newPos = True, depth = depth game + 1}
                  
                  
                    

goDown :: Game -> IO Game
goDown game = 
  if getCurrentItemType (pos game) == "Debris" then do
    displayString "blocked way"
    return game
  else 
    case pos game of
    (c, T _ _ Empty _) -> do
                               displayString "out of bounds"
                               return game
    (c,T x t1 t2 t3) -> return game { pos = incrementNodeVisits (T1 x t1 c t3,t2), newPos = True, depth = depth game + 1}
  

goRight :: Game -> IO Game
goRight game =
  if getCurrentItemType (pos game) == "Debris" then do
    displayString "blocked way"
    return game
  else
    case pos game of
    (c, T _ _ _ Empty ) -> do
                          displayString "out of bounds"
                          return game           
    (c,T x t1 t2 t3) -> return game { pos = incrementNodeVisits (T2 x t1 t2 c,t3), newPos = True, depth = depth game + 1}
  


goUp :: Game -> IO Game
goUp game = case pos game of
    (T0 x c t2 t3,t) -> return game { pos = incrementNodeVisits (c,T x t t2 t3), newPos = True}           
    (T1 x t1 c t3,t) -> return game { pos = incrementNodeVisits (c,T x t1 t t3), newPos = True}
    (T2 x t1 t2 c,t) -> return game { pos = incrementNodeVisits (c,T x t1 t2 t), newPos = True, depth = depth game - 1}           
    (Hole,t) -> do                                                                                   
         displayString "blocked entrance"
         return game


--- Implement Interact commands
  
                
useItem :: Item -> Game -> IO Game
useItem x game = 
   case getItemType x of
    "Key" -> useKey x game
    "Shovel" -> useShovel game
    "Oxygen" -> useOxygenTank game
    _ -> do
      displayString "dont have object"
      return game

    
getItem :: Game -> IO Game
getItem game =
  case getCurrentItemType game of
    "Key" -> pickUpKey game
    "Chest" -> checkChest game
    _ -> do
      displayString "no objects"
      return game

checkInventory :: Game -> IO Game
checkInventory game =
  if null (inventory game) then do
    displayString "empty inventory"
    return game
  else do
    putStrLn $ getString "inventory contains" ++ showItemList (inventory game)
    return game
    
--- Miscelaneous

quitGame :: Game -> IO Game
quitGame game = do
    displayString "quit"
    putStrLn $ drawTerZip $ getExploredMap $ pos game
    return game {gameOver = True}

retakeCommand :: Game -> IO Game
retakeCommand game = do
    displayString "parseError"
    return game

helpGamer :: Game -> IO Game
helpGamer game = do
    displayString "helpString"
    return game

showOxygen :: Game -> IO Game
showOxygen game = do
  displayStringWithVal "oxygenLeft" (oxygen game)
  return game








