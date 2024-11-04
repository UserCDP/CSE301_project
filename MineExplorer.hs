module MineExplorer where

import GameData
import Scripts
import Messages
import Parser
import Cmd
import System.IO
import Control.Concurrent.Async (race)
import System.Timeout (timeout)


--- The game loop.     

go :: Game -> IO (Maybe Game)
go game1 = do
  showCurrentNode game1
  let game = game1 {newPos = False}
  putStr "> "                                      
  hFlush stdout
  result <- race (checkOxygen game) getLine  
  case result of
    Left _ -> return Nothing
    Right line -> do
      newGame <- executeCommand (parseInput parseCmd line) game
      if (gameOver newGame) then return Nothing else go newGame
  

main :: IO (Maybe Game)
main = do
  displayString "welcome"
  initialGameState <- resetGame a_tree
  go initialGameState
