module Messages where
    
import System.IO
import GameData
import Bin
import qualified Data.Map.Strict as Map


messagesDict :: Map.Map String String
messagesDict = 
    Map.insert "welcome" "OH NOOO! You have been thrown into a dangerous mine. Oxigen is running low, find a way to escape! \n Type h for help" $
    Map.insert "blocked door" "Can't go through locked door" $
    Map.insert "out of bounds" "You cannot climb any further." $
    Map.insert "blocked entrance" "Find another way" $
    Map.insert "new key" "You picked up a new key" $
    Map.insert "unlocked" "Door unlocked" $
    Map.insert "locked" "Door locked" $
    Map.insert "quit" "Quiting already?\n Here's the progress:" $
    Map.insert "parseError" "Sorry, I don't understand" $
    Map.insert "helpString" "This is a text adventure game" $
    Map.insert "Trapdoor" "There is a trapdoor here, go find its key." $
    Map.insert "Key" "There's a key here, it might be useful ..." $
    Map.insert "helpString" "This is a text adventure game" $
    Map.insert "initialVisit" "" $
    Map.insert "familiarPlace" "This place seems familiar ... " $
    Map.insert "lost" "You're definitely lost. " $
    Map.insert "" "Explore further" $
    Map.insert "suffocated" "\n You suffocated to death :(" $
    Map.insert "oxigenLeft" "Oxigen left: " $
    Map.insert "pula" "pizda" Map.empty
    

getString :: String -> String
getString x = case Map.lookup x messagesDict of 
                Just msg -> msg
                Nothing -> "Not supposed to get this message"

displayString :: String -> IO ()
displayString str = putStrLn $ getString str

displayStringWithVal :: Show a => String -> a -> IO ()
displayStringWithVal str x = putStrLn $ getString str ++ show x


