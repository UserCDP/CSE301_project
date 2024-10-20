module GameData where

import Bin
import Items


--- This is the most import data. It holds all the game state
data Game = Game {gameOver :: Bool, pos :: BinZip Item, newPos :: Bool, inventory :: [Id]}

data Cmd = Go_Left | Go_Right | Go_Up | Meditate Int | Quit | Interact
  deriving (Show,Read)

--- Default Game state

initialGameState = Game { gameOver = False, pos = (Hole, a_tree), newPos = False, inventory = [] } where
a_tree = B N (B N (B N (L (Key 1)) (L (Key 2))) (B (Trapdoor 1) (L N) (L N ))) (L N)




