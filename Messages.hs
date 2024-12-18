module Messages where
    
import System.IO
import GameData
import Bin
import qualified Data.Map.Strict as Map


messagesDict :: Map.Map String String
messagesDict = 
    Map.insert "welcome" "Oh no! Someone locked you up in the tree-like catacombs. \nFind a way to open the trap door above with a key and escape before you go mad! \nType help for help. " $
    Map.insert "blocked way" "The way is blocked by debris. " $
    Map.insert "out of bounds" "Cannot descend through here. " $
    Map.insert "hit a wall" "There's a wall here, try another direction. " $
    Map.insert "blocked entrance" "Find another way. " $
    Map.insert "no up" "Can't climb up through here. " $
    Map.insert "new key" "You picked up a new key. " $
    Map.insert "unlocked chest" "Chest unlocked, don't forget to take its contents! " $
    Map.insert "wrong key" "Key doesn't work, perhaps try another one. " $
    Map.insert "chest already open" "The chest is already open. " $
    Map.insert "nothing to open" "There's nothing to open. " $
    Map.insert "emptying chest" "Picked up chest content. Check inventory for the new items. Type i or inventory for your inventory. " $
    Map.insert "no chest here" "There's no chest here. " $
    Map.insert "empty chest" "The chest is empty. " $
    Map.insert "locked chest" "Chest is locked. " $
    Map.insert "dont have key" "You don't have that key. " $
    Map.insert "no key here" "There is no key to pick up here. " $
    Map.insert "clear way" "Debris removed! " $
    Map.insert  "shovel is useless" "The way is clear, no need for a shovel. " $
    Map.insert "no shovel" "You don't have any shovels. " $
    Map.insert "quit" "Quiting already? \nHere's the progress: " $
    Map.insert "parseError" "Sorry, please try again. Use h or help for some help. " $
    Map.insert "helpString" "Type 'move', 'get', 'use', 'inventory', or 'sanity' for the various commands. \nThe command 'move' is followed by 'left', 'right', 'down', or 'up'. \nThe command 'get' is followed by 'item' in order to pick up any item at the location. \n'use' requires either the item followed by an ID (such as a key ID) or simply the item to use it. \n'inventory' displays the inventory and, most impotantly, 'sanity' displays the remaining units of sanity until you go mad. " $
    Map.insert "Debris" "The way is blocked by debris, use a shovel. " $
    Map.insert "Key" "There's a key here, it might be useful. " $
    Map.insert "Locked Chest" "There is a locked chest here with a key hole. " $
    Map.insert "Unlocked Full Chest" "There's an open chest here with items. " $
    Map.insert "Unlocked Empty Chest" "There's an empty chest here. " $
    Map.insert "initialVisit" "This seems relatively new. " $
    Map.insert "familiarPlace" "This place seems familiar, like you've already visited it. " $
    Map.insert "lost" "You've been here already, you're certain. " $
    Map.insert "" "" $
    Map.insert "no objects" "Nothing of use here. "  $
    Map.insert "dont have object" "You don't have that object. " $
    Map.insert "empty inventory" "Your inventory is currently empty. " $
    Map.insert "inventory contains" "Your inventory contains: "$
    Map.insert "suffocated" "\nYou have gone mad in the crypt! You couldn't exit in time. " $
    Map.insert "oxygenLeft" "Sanity left: " $
    Map.insert "atLeaf" "You cannot go down any further from here." $
    Map.insert "not won yet" "This is the wrong place to use the key on! Go back to the entrance of the catacombs." $
    Map.insert "no master" "You do not have the Masterkey to open the entrance of the catacombs." $
    Map.insert "winMessage" "Congratulations! \nYou have made it out of the crypt alive and with some sanity left in you. \nYou alerted the police of what happened and the serial killer that \nthrew you in the catacombs was apprehended by local law enforcement and will face 40 years in prison." $
    Map.insert "used oxygen tank" "Oxygen tank consumed. Your sanity has increased with a breath of fresh oxygen (even though it's air, not pure oxygen, you'd die otherwise,\nI mean seriously, who would say 'oxygen', you breathe air, not oxygen, nobody breathes oxygen,\nwell that's not true, and they don't even actually have oxygen tanks in crypts, how did you even find one? Was it just lying around?\nThat's stupidly irresponsible of whoever put that oxygen tank there, it's a fire hazard after all). " $
    Map.insert "dont have oxygen tank" "You don't have an oxygen tank. " $ Map.empty

    

getString :: String -> String
getString x = case Map.lookup x messagesDict of 
                Just msg -> msg
                Nothing -> "Not supposed to get this message"

displayString :: String -> IO ()
displayString str = putStrLn $ getString str

displayStringWithVal :: Show a => String -> a -> IO ()
displayStringWithVal str x = putStrLn $ getString str ++ show x
