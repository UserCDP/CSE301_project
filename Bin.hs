module Bin where

import Data.Tree

--- Data Type for map

data Ter a = Empty | T a (Ter a) (Ter a) (Ter a)
  deriving (Show,Eq, Functor)


data TerCxt a = Hole
              | T0 a (TerCxt a) (Ter a) (Ter a)
              | T1 a (Ter a) (TerCxt a) (Ter a)
              | T2 a (Ter a) (Ter a) (TerCxt a)
  deriving (Show,Eq, Functor)



type TerZip a = (TerCxt a, Ter a)


-- A function that checks in the value at the node


customfmap :: (a -> b) -> TerZip a -> TerZip b
customfmap f (c, t) = (fmap f c, fmap f t)

getValAtNode :: TerZip a -> a
getValAtNode (_,T x _ _ _) = x

insertValAtNode :: TerZip a -> a -> TerZip a
insertValAtNode (c, T x t1 t2 t3) y = (c, T y t1 t2 t3)

isNodeLeaf :: TerZip a -> Bool
isNodeLeaf (c,t) = case t of
               (c, T _ Empty Empty Empty) -> True
               _ -> False

atRoot :: TerZip a -> Bool
atRoot (c,t) = c == Hole

--- A routine used to print the visible map

data Void = Void deriving (Show,Eq)

convertBoolTer :: Ter Bool -> Ter Void
convertBoolTer Empty = Empty
convertBoolTer (T True t1 t2 t3) = T Void (convertBoolTer t1) (convertBoolTer t2) (convertBoolTer t3)
convertBoolTer (T False _ _ _) = Empty

convertBoolTerCxt :: TerCxt Bool -> TerCxt Void
convertBoolTerCxt Hole = Hole
convertBoolTerCxt (T0 False _ _ _) = Hole 
convertBoolTerCxt (T1 False _ _ _) = Hole 
convertBoolTerCxt (T2 False _ _ _) = Hole
convertBoolTerCxt (T0 True c t2 t3) = T0 Void (convertBoolTerCxt c) (convertBoolTer t2)  (convertBoolTer t3)
convertBoolTerCxt (T1 True t1 c t3) = T1 Void (convertBoolTer t1) (convertBoolTerCxt c)  (convertBoolTer t3) 
convertBoolTerCxt (T2 True t1 t2 c) = T2 Void (convertBoolTer t1) (convertBoolTer t2)  (convertBoolTerCxt c)

convertBoolTerZip :: TerZip Bool -> TerZip Void
convertBoolTerZip (c, t) = (convertBoolTerCxt c, convertBoolTer t)

getNonEmptyChildrenTer :: Eq a => Ter a -> [Ter a]
getNonEmptyChildrenTer (T _ t1 t2 t3) = [ t | t <- [t1,t2,t3], t/= Empty] 

getNonEmptyChildrenTerCxt :: Eq a => TerCxt a -> [Ter a]
getNonEmptyChildrenTerCxt Hole = []
getNonEmptyChildrenTerCxt (T0 _ c t2 t3) = [ t | t <- [t2,t3], t/= Empty]
getNonEmptyChildrenTerCxt (T1 _ t1 c t3) = [ t | t <- [t1,t3], t/= Empty] 
getNonEmptyChildrenTerCxt (T2 _ t1 t2 c) = [ t | t <- [t1,t2], t/= Empty]  

getContext :: TerCxt a -> TerCxt a
getContext Hole = Hole
getContext (T0 _ c _ _) = c
getContext (T1 _ _ c _) = c
getContext (T2 _ _ _ c) = c


getDepth :: TerZip a -> Double 
getDepth (Hole, _) = 1
getDepth (T0 x c t2 t3,t) = getDepth (c,T x t t2 t3)
getDepth (T1 x t1 c t3,t) = getDepth (c,T x t1 t t3)
getDepth (T2 x t1 t2 c,t) = 1 + getDepth (c,T x t1 t2 t)



treeFromTer :: (Eq a,Show a) => Ter a -> Tree String
treeFromTer t = Node "*" (map (\x -> treeFromTer x) (getNonEmptyChildrenTer t))

treeCxtFromTerCxt :: (Eq a,Show a) => TerCxt a -> Tree String -> Tree String
treeCxtFromTerCxt Hole      t = t
treeCxtFromTerCxt c t = treeCxtFromTerCxt (getContext c) (Node "*" $ (map (\x -> treeFromTer x) (getNonEmptyChildrenTerCxt c)) ++ [t])

treeFromTerZip :: (Eq a,Show a) => TerZip a -> Tree String
treeFromTerZip (c,t) = treeCxtFromTerCxt c (t'{rootLabel=rootLabel t' ++ marker})
  where
    t' = treeFromTer t
    marker = "@ <--you"

printTree :: Tree String -> String
printTree node = go node "" True
  where
    -- Recursive helper function to print each node
    go :: Tree String -> String -> Bool -> String
    go (Node desc cs) prefix isLast =
        -- Print the node name and description
        prefix ++ (if isLast then "└── " else "├── ") ++ " (" ++ desc ++ ")\n" ++
        -- Traverse the children with updated prefixes
        concatMap (\(c, i) -> go c (prefix ++ (if isLast then "    " else "│   ")) (i == length cs - 1)) (zip cs [0..])

drawTerZip :: (Eq a, Show a) => TerZip a -> String
drawTerZip = drawTree. treeFromTerZip
