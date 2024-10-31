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
executeCommand (Just Go_Up) = goUp
executeCommand (Just (Use x) ) = useItem x
executeCommand (Just PickUp ) = getItem 
executeCommand (Just Check_Inventory) = checkInventory
executeCommand (Just Quit) = quitGame
executeCommand (Just Show_Oxigen) = showOxigen
executeCommand (Just Help) = helpGamer
executeCommand Nothing = retakeCommand


--- Implement movement commands
goLeft :: Game -> IO Game
goLeft game = if getItemType (getCurrentItem game) == "Debris" then do
                displayString "blocked way"
                return game
                else
                  case pos game of
                    (c, B _ Empty _ ) -> do
                      displayString "out of bounds"
                      return game
                    (c,B x t1 t2) -> return game { pos = incrementNodeVisits (B0 x c t2,t1), newPos = True} 
                    

goRight :: Game -> IO Game
goRight game = if getCurrentItemType game == "Debris" then do
                displayString "blocked way"
                return game
                else
                  case pos game of
                    (c, B _ _ Empty ) -> do
                      displayString "out of bounds"
                      return game
                    (c,B x t1 t2) -> return game { pos = incrementNodeVisits (B1 x t1 c,t2), newPos = True} 

goUp :: Game -> IO Game
goUp game = case pos game of
    (B0 x c t2,t) -> return game { pos = incrementNodeVisits (c,B x t t2), newPos = True}           
    (B1 x t1 c,t) -> return game { pos = incrementNodeVisits (c,B x t1 t), newPos = True}            
    (Hole,t) -> do                                                                                   
         displayString "blocked entrance"
         return game


--- Implement Interact commands
  
                
useItem :: Item -> Game -> IO Game
useItem x game = 
   case getItemType x of
    "Key" -> useKey x game
    "Shovel" -> useShovel game
    "Oxigen" -> useOxigenTank x game
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
    putStrLn $ drawBinZip $ getExploredMap $ pos game
    return game {gameOver = True}

retakeCommand :: Game -> IO Game
retakeCommand game = do
    displayString "parseError"
    return game

helpGamer :: Game -> IO Game
helpGamer game = do
    displayString "helpString"
    return game

showOxigen :: Game -> IO Game
showOxigen game = do
  ox <- getOxigen game
  displayStringWithVal "oxigenLeft" ox
  return game








