module GameData where

import Bin
import Items
import Data.Time.Clock

--- This is the most import data. It holds all the game state

data NodeInfo = NodeInfo {item :: Item, visits :: Int}
data Game = Game {gameOver :: Bool, pos :: TerZip NodeInfo, lastMovement :: Cmd, newPos :: Bool, inventory :: [Item], initialTime :: UTCTime, oxygen :: Double, depth :: Int}

data Cmd = Go_Left | Go_Right | Go_Down | Go_Back | Go_Up | Quit | Use Item | PickUp | Show_Oxygen | Help | Check_Inventory
  deriving (Show,Read)


---- Constants

maxOxygen :: Double
maxOxygen = 300.0



