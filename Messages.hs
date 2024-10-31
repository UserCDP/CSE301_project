module Messages where
    
import System.IO
import GameData
import Bin
import qualified Data.Map.Strict as Map


messagesDict :: Map.Map String String
messagesDict = 
    Map.insert "welcome" "Oh no! You find yourself in a mine and the air is getting thin. Find a way to open the trap and escape before you suffocate! \n Type h for help." $
    Map.insert "blocked way" "The way is blocked by debris" $
    Map.insert "out of bounds" "Cannot descend any further." $
    Map.insert "blocked entrance" "Find another way " $
    Map.insert "new key" "You picked up a new key " $
    Map.insert "unlocked chest" "Chest unlocked " $
    Map.insert "wrong key" "key doesn't work, perhaps try another one " $
    Map.insert "chest already open""The chest is already open " $
    Map.insert "nothing to open" "There's nothing to open " $
    Map.insert "emptying chest" "Picked up chest content" $
    Map.insert "no chest here" "There's no chest here" $
    Map.insert "empty chest" "The chest is empty" $
    Map.insert "locked chest" "Chest is locked" $
    Map.insert "dont have key" "You don't have that key " $
    Map.insert "no key here" "There is no key to pick up here " $
    Map.insert "clear way" "Debris removed! " $
    Map.insert  "shovel is useless" "The way is clear, no need for a shovel ..." $
    Map.insert "no shovel" "You dont have any shovels " $
    Map.insert "quit" "Quiting already?\n Here's the progress:" $
    Map.insert "parseError" "Sorry, I don't understand" $
    Map.insert "helpString" "Use 'go' to move down, left or right such as 'go left' or 'go down'." $
    Map.insert "Debris" "The way is blocked by debris. Use a Shovel ..." $
    Map.insert "Key" "There's a key here, it might be useful ..." $
    Map.insert "Locked Chest" "There is a locked chest here. Go find a key to open it ..." $
    Map.insert "Unlocked Full Chest" "There's an open chest here. Go check it ..." $
    Map.insert "Unlocked Empty Chest" "There's an empty chest here ..." $
    Map.insert "initialVisit" "" $
    Map.insert "familiarPlace" "This place seems familiar ... " $
    Map.insert "lost" "You're definitely lost. " $
    Map.insert "" "Explore further" $
    Map.insert "no objects" "Nothing of use here ... "  $
    Map.insert "dont have object" "You dont have that object" $
    Map.insert "empty inventory" "Your inventory is currently empty " $
    Map.insert "inventory contains" "Your inventory contains: "$
    Map.insert "suffocated" "\n You suffocated to death :(" $
    Map.insert "oxygenLeft" "Oxygen left: " $
    Map.insert "used oxygen tank" "Oxygen tank consumed" $
    Map.insert "dont have oxygen tank" "You dont have that oxygen tank" $
    Map.insert "pula" "pizda" Map.empty
    

getString :: String -> String
getString x = case Map.lookup x messagesDict of 
                Just msg -> msg
                Nothing -> "Not supposed to get this message"

displayString :: String -> IO ()
displayString str = putStrLn $ getString str

displayStringWithVal :: Show a => String -> a -> IO ()
displayStringWithVal str x = putStrLn $ getString str ++ show x
