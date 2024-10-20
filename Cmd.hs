module Cmd where

import GameData
import Items
import Bin



--- Translates Name of the command into an actual function
executeCommand :: Maybe Cmd -> (Game -> IO Game)
executeCommand (Just Go_Left) = goLeft
executeCommand (Just Go_Right) = goRight
executeCommand (Just Go_Up) = goUp
executeCommand (Just Interact) = useItem
executeCommand (Just Quit) = quitGame
executeCommand Nothing = doNothing


--- Implement movement commands
goLeft :: Game -> IO Game
goLeft game = if getItemTypeAtNode (pos game) == "Trapdoor" then do
                putStrLn "Can't go through locked door"
                return game {newPos = False}
                else
                  case pos game of
                    (c,B x t1 t2) -> return game { pos = (B0 x c t2,t1), newPos = True} -- climb up to the left
                    (c,L _) -> do
                      putStrLn "You cannot climb any further."
                      return game {newPos = False}

goRight :: Game -> IO Game
goRight game = if getItemTypeAtNode (pos game) == "Trapdoor" then do
                putStrLn "Can't go through locked door"
                return game {newPos = False}
                else
                  case pos game of
                    (c,B x t1 t2) -> return game { pos = (B1 x t1 c,t2), newPos = True} -- climb up to the left
                    (c,L _) -> do
                      putStrLn "You cannot climb any further."
                      return game {newPos = False}

goUp :: Game -> IO Game
goUp game = case pos game of
    (B0 x c t2,t) -> return game { pos = (c,B x t t2), newPos = True}            -- climb down from the left, or
    (B1 x t1 c,t) -> return game { pos = (c,B x t1 t), newPos = True}            -- climb down from the right, or
    (Hole,t) -> do                                                               -- already at the root
         putStrLn "Find another way"
         return game {newPos = False}


--- Implement Interact commands
                
useItem :: Game -> IO Game
useItem game = case val (pos game) of
    Key id -> if not (id `elem` inventory game) then do
        putStrLn "You picked up a new key"
        return game { pos = insertVal (pos game) N, inventory = (id : inventory game), newPos = False }
        else return game {newPos = False}
  
    Trapdoor id -> if (id`elem` inventory game) then do
        putStrLn "Door unlocked"
        return game { pos = insertVal (pos game) N, newPos = False }
        else do
             putStrLn "Door locked"
             return game {newPos = False}          

    N -> return game {newPos = False}


--- Miscelaneous

quitGame :: Game -> IO Game
quitGame game = do
    putStrLn "Okay."
    putStrLn "You ended the game over here:\n"
    putStrLn (drawBinZip (pos game))
    putStrLn "Goodbye."
    return game {gameOver = True}

doNothing :: Game -> IO Game
doNothing game = do
    putStrLn "I'm sorry, I do not understand."
    return game {newPos = False}






