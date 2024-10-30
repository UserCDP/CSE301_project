module Bin where

import Data.Tree

--- Data Type for map

data Bin a = Empty | B a (Bin a) (Bin a)
  deriving (Show,Eq, Functor)


data BinCxt a = Hole
              | B0 a (BinCxt a) (Bin a)
              | B1 a (Bin a) (BinCxt a)
  deriving (Show,Eq, Functor)



type BinZip a = (BinCxt a, Bin a)


-- A function that checks in the value at the node


customfmap :: (a -> b) -> BinZip a -> BinZip b
customfmap f (c, t) = (fmap f c, fmap f t)

getValAtNode :: BinZip a -> a
getValAtNode (_,B x _ _) = x

insertValAtNode :: BinZip a -> a -> BinZip a
insertValAtNode (c, B x t1 t2) y = (c, B y t1 t2)


--- A routine used to print the visible map

data Void = Void deriving Show

convertBoolBin :: Bin Bool -> Bin Void
convertBoolBin Empty = Empty
convertBoolBin (B True t1 t2) = B Void (convertBoolBin t1) (convertBoolBin t2)
convertBoolBin (B False _ _) = Empty

convertBoolBinCxt :: BinCxt Bool -> BinCxt Void
convertBoolBinCxt Hole = Hole
convertBoolBinCxt (B0 False _ _) = Hole 
convertBoolBinCxt (B1 False _ _) = Hole 
convertBoolBinCxt (B0 True c t) = B0 Void (convertBoolBinCxt c) (convertBoolBin t)
convertBoolBinCxt (B1 True t c) = B1 Void (convertBoolBin t) (convertBoolBinCxt c) 

convertBoolBinZip :: BinZip Bool -> BinZip Void
convertBoolBinZip (c, t) = (convertBoolBinCxt c, convertBoolBin t)



treeFromBin :: Show a => Bin a -> Tree String
treeFromBin (B _ Empty Empty) = Node "*" []
treeFromBin (B _ t1 Empty) =  Node "*" [treeFromBin t1]
treeFromBin (B _ Empty t2)     = Node "*" [treeFromBin t2]
treeFromBin (B x t1 t2) = Node "*" [treeFromBin t1,treeFromBin t2]

treeCxtFromBinCxt :: Show a => BinCxt a -> Tree String -> Tree String
treeCxtFromBinCxt Hole      t = t
treeCxtFromBinCxt (B0 x c Empty) t = treeCxtFromBinCxt c (Node "*" [t])
treeCxtFromBinCxt (B0 x c t2) t = treeCxtFromBinCxt c (Node "*" [t, treeFromBin t2])
treeCxtFromBinCxt (B1 x Empty c) t = treeCxtFromBinCxt c (Node "*" [t])
treeCxtFromBinCxt (B1 x t1 c) t = treeCxtFromBinCxt c (Node "*" [treeFromBin t1, t])

treeFromBinZip :: Show a => BinZip a -> Tree String
treeFromBinZip (c,t) = treeCxtFromBinCxt c (t'{rootLabel=rootLabel t' ++ marker})
  where
    t' = treeFromBin t
    marker = "@ <--you"

drawBin :: Show a => Bin a -> String
drawBin = drawTree . treeFromBin

drawBinZip :: Show a => BinZip a -> String
drawBinZip = drawTree . treeFromBinZip
