module GameData where

import Bin
import Items
import Data.Time.Clock

--- This is the most import data. It holds all the game state

data NodeInfo = NodeInfo {item :: Item, visits :: Int}
data Game = Game {gameOver :: Bool, pos :: BinZip NodeInfo, newPos :: Bool, inventory :: [Item], initialTime :: UTCTime, oxigenBoost :: Double}

data Cmd = Go_Left | Go_Right | Go_Up | Meditate Int | Quit | Use Item | PickUp | Show_Oxigen | Help | Check_Inventory
  deriving (Show,Read)


---- Constants

maxOxigen :: Double
maxOxigen = 300.0




