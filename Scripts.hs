module Scripts where

import GameData
import System.IO
import Bin
import Items
import Messages
import Data.Time.Clock



--- Reseting the game

a_tree :: Ter Item
a_tree = T N (T N (T (Key 1) Empty Empty Empty) (T N (T N Empty Empty Empty) (T N Empty Empty Empty) Empty) Empty) 
             (T N (T N Empty Empty Empty) (T N Empty (T N Empty (T Debris Empty (T N Empty (T N Empty (T (Chest False 2 [Masterkey]) Empty Empty Empty) Empty) Empty) Empty) Empty) Empty) (T N (T Debris (T N Empty Empty Empty) Empty (T (Key 2) Empty Empty Empty)) Empty Empty))
             (T N (T N Empty (T N Empty (T (Chest False 1 [Shovel, OxygenTank]) Empty Empty Empty) Empty) (T N Empty Empty Empty)) (T N Empty (T N Empty Empty Empty) Empty) (T (Key 1) Empty Empty (T N Empty Empty Empty)))
a_tree = T N Empty (T N Empty (T N Empty (T N (T (Key 1) Empty Empty Empty) Empty (T (Chest False 1 [OxygenTank]) Empty Empty Empty)) (T N Empty Empty Empty) ) Empty) Empty

    
    
resetGame :: Ter Item -> IO Game
resetGame a = do
    x <- getCurrentTime
    return Game { gameOver = False, pos = resetMap a , newPos = False, inventory = [], initialTime = x, oxygen = maxOxygen, depth = 1}


---- The Oxygen Logic

updateOxygen :: Game -> IO Game
updateOxygen game = do
    now <- getCurrentTime
    let previousTime = (initialTime game)
    let diff = realToFrac (diffUTCTime now previousTime)
    let change = diff * (fromIntegral (depth game))
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

isPlayerAtRoot :: Game -> Bool
isPlayerAtRoot game = atRoot (pos game)


getItemAtNode :: TerZip NodeInfo -> Item
getItemAtNode b = item (getValAtNode b)

getVisitsAtNode :: TerZip NodeInfo -> Int
getVisitsAtNode b = visits (getValAtNode b)

removeItemAtNode :: TerZip NodeInfo -> TerZip NodeInfo
removeItemAtNode b = insertValAtNode b ((getValAtNode b) {item = N})


unlockChestAtNode :: TerZip NodeInfo -> TerZip NodeInfo
unlockChestAtNode b = 
    let v = (getValAtNode b) in
    case item v of
        Chest False id obj -> insertValAtNode b (v{item = Chest True id obj })
        _ -> b

emptyChestAtNode :: TerZip NodeInfo -> TerZip NodeInfo
emptyChestAtNode b =
    let v = (getValAtNode b) in
    case item v of
        Chest True id obj -> insertValAtNode b (v{item = Chest True id [] })
        _ -> b

checkYouHaveItem :: String -> Game -> Bool
checkYouHaveItem x game = checkListContainsItemType x (inventory game)

incrementNodeVisits :: TerZip NodeInfo -> TerZip NodeInfo
incrementNodeVisits b = insertValAtNode b ((getValAtNode b) {visits = visits (getValAtNode b) + 1})

resetMap :: Ter Item -> TerZip NodeInfo 
resetMap b = incrementNodeVisits (Hole, fmap (\x -> NodeInfo{item = x, visits = 0}) b)

getExploredMap :: TerZip NodeInfo -> TerZip Void
getExploredMap b = convertBoolTerZip (customfmap (\x-> visits x > 0) b)

getCurrentItem :: Game -> Item
getCurrentItem game = getItemAtNode (pos game)

getCurrentItemType :: Game -> String
getCurrentItemType game = getItemType $ getCurrentItem game





----- Print stuff when reaching a node

showCurrentNode :: Game -> IO ()
showCurrentNode game =
    if newPos game then
        putStrLn $ (showVisits game) ++ showItem (getCurrentItem game) ++ "\nYou're currently at depth " ++ show(depth game)
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



---- Winning function

winGame :: Game -> IO Game
winGame = do
    displayString "winMessage"
    putStrLn $ drawTerZip $ getExploredMap $ pos game
    return game {gameOver = True}

