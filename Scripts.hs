module Scripts where

import GameData
import System.IO
import Bin
import Items
import Messages
import Data.Time.Clock



--- Reseting the game

a_tree :: Bin Item
a_tree = B N (B N (B N (B (Key 1) Empty Empty) (B (Key 2) Empty Empty)) (B (Trapdoor 1) (B N Empty Empty) (B N Empty Empty ))) (B N Empty Empty)

resetGame :: Bin Item -> IO Game
resetGame a = do
    x <- getCurrentTime
    return Game { gameOver = False, pos = resetMap a , newPos = False, inventory = [], initialTime = x}


---- The Oxigen Logic

getOxigen :: Game -> IO Double
getOxigen game = do
    now <- getCurrentTime
    let ox = realToFrac (diffUTCTime now (initialTime game))
    return (maxOxigen - ox)


checkOxigen :: Game -> IO ()
checkOxigen game = do
    ox <- getOxigen game
    if ox < 0 
        then do
             displayString "suffocated"
             return ()
    else checkOxigen game


---- Funtions manipulating nodes

getItemAtNode :: BinZip NodeInfo -> Item
getItemAtNode b = item (getValAtNode b)

getVisitsAtNode :: BinZip NodeInfo -> Int
getVisitsAtNode b = visits (getValAtNode b)

consumeItemAtNode :: BinZip NodeInfo ->BinZip NodeInfo
consumeItemAtNode b = insertValAtNode b ((getValAtNode b) {item = N})

incrementNodeVisits :: BinZip NodeInfo -> BinZip NodeInfo
incrementNodeVisits b = insertValAtNode b ((getValAtNode b) {visits = visits (getValAtNode b) + 1})

resetMap :: Bin Item -> BinZip NodeInfo
resetMap b = incrementNodeVisits (Hole, fmap (\x -> NodeInfo{item = x, visits = 0}) b)

getExploredMap :: BinZip NodeInfo -> BinZip Void
getExploredMap b = convertBoolBinZip (customfmap (\x-> visits x > 0) b)

getCurrentItem :: Game -> Item
getCurrentItem game = getItemAtNode (pos game)



----- Print stuff when reaching a node

showCurrentNode :: Game -> IO ()
showCurrentNode game =
    if newPos game then
        putStrLn ((showVisits game) ++ getString (getItemType ( getItemAtNode ( pos game))))
    else return ()

showVisits :: Game -> String
showVisits game 
   | v < 2 = getString "initialVisit"
   | v > 1 && v < 5 = getString "familiarPlace"
   | v > 4 = getString "lost"
   where 
    v = visits $ getValAtNode $ pos game