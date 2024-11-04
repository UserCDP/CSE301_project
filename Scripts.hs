module Scripts where

import GameData
import System.IO
import Bin
import Items
import Messages
import Data.Time.Clock



--- Reseting the game

a_tree :: Bin NodeInfo
a_tree = B NodeInfo{item=N,depth = 1} (B NodeInfo{item=N,depth = 15} Empty Empty) (B NodeInfo{item=N,depth = 1} (B NodeInfo{item=N,depth = 15} Empty Empty) (B NodeInfo{item=N,depth=1} Empty Empty))
    
    
resetGame :: Bin NodeInfo -> IO Game
resetGame a = do
    x <- getCurrentTime
    return Game { gameOver = False, pos = resetMap a , newPos = False, inventory = [], initialTime = x, oxygen = maxOxygen}


---- The Oxygen Logic

updateOxygen :: Game -> IO Game
updateOxygen game = do
    now <- getCurrentTime
    let previousTime = (initialTime game)
    let diff = realToFrac (diffUTCTime now previousTime)
    let change = diff * getDepthAtNode (pos game)
    return game{initialTime = now, oxygen = (oxygen game) - change}

checkOxygen :: Game -> IO ()
checkOxygen game1 = do
    game <- updateOxygen game1
    if (oxygen game) < 0 
        then do
             displayString "suffocated"
             return ()
    else checkOxygen game


---- Functions manipulating nodes

getDepthAtNode :: BinZip NodeInfo -> Double
getDepthAtNode b = depth (getValAtNode b)

getItemAtNode :: BinZip NodeInfo -> Item
getItemAtNode b = item (getValAtNode b)

getVisitsAtNode :: BinZip NodeInfo -> Int
getVisitsAtNode b = visits (getValAtNode b)

removeItemAtNode :: BinZip NodeInfo -> BinZip NodeInfo
removeItemAtNode b = insertValAtNode b ((getValAtNode b) {item = N})


unlockChestAtNode :: BinZip NodeInfo -> BinZip NodeInfo
unlockChestAtNode b = 
    let v = (getValAtNode b) in
    case item v of
        Chest False id obj -> insertValAtNode b (v{item = Chest True id obj })
        _ -> b

emptyChestAtNode :: BinZip NodeInfo -> BinZip NodeInfo
emptyChestAtNode b =
    let v = (getValAtNode b) in
    case item v of
        Chest True id obj -> insertValAtNode b (v{item = Chest True id [] })
        _ -> b

checkYouHaveItem :: String -> Game -> Bool
checkYouHaveItem x game = checkListContainsItemType x (inventory game)

incrementNodeVisits :: BinZip NodeInfo -> BinZip NodeInfo
incrementNodeVisits b = insertValAtNode b ((getValAtNode b) {visits = visits (getValAtNode b) + 1})

resetMap :: Bin NodeInfo -> BinZip NodeInfo
resetMap b = incrementNodeVisits (Hole, fmap (\x -> x{item = (item x), visits = 0}) b)

getExploredMap :: BinZip NodeInfo -> BinZip Void
getExploredMap b = convertBoolBinZip (customfmap (\x-> visits x > 0) b)

getCurrentItem :: Game -> Item
getCurrentItem game = getItemAtNode (pos game)

getCurrentItemType :: Game -> String
getCurrentItemType game = getItemType $ getCurrentItem game



----- Print stuff when reaching a node

showCurrentNode :: Game -> IO ()
showCurrentNode game =
    if newPos game then
        putStrLn $ (showVisits game) ++ showItem (getCurrentItem game)
    else return ()

showVisits :: Game -> String
showVisits game 
   | v < 5 = getString "initialVisit"
   | v > 4 && v < 7 = getString "familiarPlace"
   | v > 6 = getString "lost"
   where 
    v = visits $ getValAtNode $ pos game

showItem :: Item -> String
showItem x =
  case x of
    Chest False _ _ -> getString "Locked Chest"
    Chest True _ [] -> getString "Unlocked Empty Chest"
    Chest True _ _ -> getString "Unlocked Full Chest"
    _ -> getString $ getItemType x
