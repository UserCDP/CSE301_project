module Cmd where

import Bin

data Cmd = Go_Left | Go_Right | Go_Down | Meditate Int | Quit | Action
  deriving (Show,Read)

data Obj = N | Key Int | Trapdoor Int    deriving Show

--- Function that gets the type of object
fromObj :: Obj -> Int
fromObj N = 0
fromObj (Key _) = 1
fromObj (Trapdoor _) = 2

type Player = (BinZip Obj, [Int])


-- a small binary tree
a_tree :: Bin Obj
a_tree = B N (B N (B N (L (Key 1)) (L (Key 2))) (B (Trapdoor 1) (L N) (L N ))) (L N)

player :: Player
player = ((Hole,a_tree), [])

