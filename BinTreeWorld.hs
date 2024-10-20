import GameData
import Cmd
import Bin
import Parser
import Items
import System.IO
import Control.Concurrent (threadDelay)


--- We want to show the text explaining the properties of our current node just when we enter it. The function below shows this text and then sets
--- the variable newPos to False so that the text wont show up until we get to a new node.

showUponVisit :: Game -> IO Game
showUponVisit game = if newPos game 
  then case getItemTypeAtNode (pos game) of
    "Nothing" -> case atLeaf (pos game) of
                      True -> do putStrLn "You've reached the bottom. Climb up" 
                                 return game {newPos = False}
                      False -> do putStrLn "Explore further" 
                                  return game {newPos = False}
    "Trapdoor" -> do putStrLn "There is a trapdoor here, go find its key" 
                     return game {newPos = False}
    "Key" -> do putStrLn "There's a key here, it might be useful ..." 
                return game {newPos = False}
  else 
    return game {newPos = False}



--- The game loop.     

go :: Game -> IO (Maybe Game)
go game = do
  showUponVisit game
  putStr "> "                                      -- print the prompt
  hFlush stdout
  line <- getLine  
  newGame <- executeCommand (parseInput parseCmd line) game        -- flush standard output
  if (gameOver newGame) then return Nothing else go newGame


main :: IO (Maybe Game)
main = do
  putStrLn "OH NOOO! You have been thrown into a dangerous mine. Find a way to escape, and quick!"
  go initialGameState



