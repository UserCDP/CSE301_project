module GameData where

import Bin
import Items
import Data.Time.Clock

--- This is the most import data. It holds all the game state

data NodeInfo = NodeInfo {item :: Item, visits :: Int}
data Game = Game {gameOver :: Bool, pos :: BinZip NodeInfo, newPos :: Bool, inventory :: [Id], initialTime :: UTCTime}

data Cmd = Go_Left | Go_Right | Go_Up | Meditate Int | Quit | Interact | Show_Oxigen | Help
  deriving (Show,Read)


---- Constants

maxOxigen :: Double
maxOxigen = 50.0




