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
    go (z,k, visited) = do  
      if visited
        then case fromObj(val z) of 
          0 -> case z of
            (_,B _ _ _) -> do putStrLn "Explore further" 
                              go (z,k, False)
            (_,L _)-> do putStrLn "You've reached the bottom. Climb up"  
                         go (z,k, False)                                       
          1 -> do putStrLn "There's a key here, it might be useful ..." 
                  go (z,k, False)
          2 -> do putStrLn "There is a trapdoor here, go find its key" 
                  go (z,k, False)
          
      else do 
        putStr "> "                                      -- print the prompt
        hFlush stdout                                    -- flush standard output
        line <- getLine                                  -- get a line of input
        case parseInput parseCmd line of                 -- parse the input
            Nothing -> do
              putStrLn "I'm sorry, I do not understand."
              go (z,k,False)

            Just Go_Left -> 
              if fromObj (val z) == 2 then do
                putStrLn "Can't go through locked door"
                go (z,k, False)
                else
                  case z of
                    (c,B x t1 t2) -> go ((B0 x c t2,t1), k, True)           -- climb up to the left
                    (c,L _) -> do
                      putStrLn "You cannot climb any further."
                      go (z,k, False)

            Just Go_Right ->
              if fromObj (val z) == 2 then do
                putStrLn "Can't go through locked door"
                go (z,k, False)
                else
                  case z of
                    (c,B x t1 t2) -> go ((B1 x t1 c,t2),k, True)           -- climb up to the right
                    (c,L _) -> do
                      putStrLn "You cannot climb any further."
                      go (z,k,False)

            Just Go_Down ->
              case z of
                (B0 x c t2,t) -> go ((c,B x t t2),k, True)             -- climb down from the left, or
                (B1 x t1 c,t) -> go ((c,B x t1 t),k, True)             -- climb down from the right, or
                (Hole,t) -> do                           -- already at the root
                  putStrLn "Dont spend time in vain, Pier has locked you well"
                  go (z,k,False)

            Just (Meditate n) -> do
              putStrLn "You close your eyes and focus on your surroundings."
              threadDelay (n * 1000000)
              putStrLn "You open your eyes."
              go (z,k, False)

            Just Action -> 
              case val z of
                Key x -> if (x `elem` k) == False then do
                  putStrLn "You picked up a new key"
                  go (insertVal z N, k ++ [x], False)
                  else go (z,k, False)
              
                Trapdoor x -> if (x `elem` k) then do
                  putStrLn "Door unlocked"
                  go (insertVal z N, k, False)
                  else do
                    putStrLn "Door locked"
                    go (z,k, False)

                N -> go (z,k, False)

            Just Quit -> do
              putStrLn "Okay."
              putStrLn "You ended the game over here:\n"
              putStrLn (drawBinZip z)
              putStrLn "Goodbye."
              return ()



main = repl
