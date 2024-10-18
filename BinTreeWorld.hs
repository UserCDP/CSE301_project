import Bin
import Cmd
import Parser

import System.IO
import Control.Concurrent (threadDelay)



-- the top-level interactive loop
repl :: IO ()
repl = do
  putStrLn "OH NOOO! The evil Pierre Yves has thrown you into a dangerous mine. Find a way to escape, and quick!"
  go (player)
  where
    go :: Player -> IO ()
    go (z,k) = do  
      case fromObj(val z) of 
        0 -> case z of
          (_,B _ _ _) -> putStrLn "Explore further" 
          (_,L _)-> putStrLn "You've reached the bottom. Climb up"                                        
        1 -> putStrLn "There's a key here, it might be useful ..."
        2 -> putStrLn "There is a trapdoor here, go find its key" 
        
                                                
      putStr "> "                                      -- print the prompt
      hFlush stdout                                    -- flush standard output
      line <- getLine                                  -- get a line of input
      case parseInput parseCmd line of                 -- parse the input
          Nothing -> do
            putStrLn "I'm sorry, I do not understand."
            go (z,k)

          Just Go_Left -> 
            if fromObj (val z) == 2 then do
              putStrLn "Can't go through locked door"
              go (z,k)
              else
                case z of
                  (c,B x t1 t2) -> go ((B0 x c t2,t1), k)           -- climb up to the left
                  (c,L _) -> do
                    putStrLn "You cannot climb any further."
                    go (z,k)

          Just Go_Right ->
            if fromObj (val z) == 2 then do
              putStrLn "Can't go through locked door"
              go (z,k)
              else
                case z of
                  (c,B x t1 t2) -> go ((B1 x t1 c,t2),k)           -- climb up to the right
                  (c,L _) -> do
                    putStrLn "You cannot climb any further."
                    go (z,k)

          Just Go_Down ->
            case z of
              (B0 x c t2,t) -> go ((c,B x t t2),k)             -- climb down from the left, or
              (B1 x t1 c,t) -> go ((c,B x t1 t),k)             -- climb down from the right, or
              (Hole,t) -> do                           -- already at the root
                putStrLn "You are already at the root."
                putStrLn "You cannot climb down any further."
                go (z,k)

          Just (Meditate n) -> do
            putStrLn "You close your eyes and focus on your surroundings."
            threadDelay (n * 1000000)
            putStrLn "You open your eyes."
            go (z,k)

          Just Action -> 
            case val z of
              Key x -> if (x `elem` k) == False then do
                putStrLn "You picked up a new key"
                go (insertVal z N, k ++ [x])
                else go (z,k)
            
              Trapdoor x -> if (x `elem` k) then do
                putStrLn "Door unlocked"
                go (insertVal z N, k)
                else do
                  putStrLn "Door locked"
                  go (z,k)

              N -> go (z,k)

          Just Quit -> do
            putStrLn "Okay."
            putStrLn "You ended the game over here:\n"
            putStrLn (drawBinZip z)
            putStrLn "Goodbye."
            return ()



main = repl
