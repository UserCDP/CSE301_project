{-# LANGUAGE OverloadedStrings #-}
module MineExplorer where

import GameData
import Scripts
import Messages
import Parser
import Cmd
import System.IO
import Control.Concurrent.Async (race)
import System.Timeout (timeout)

import SDL
import SDL.Mixer as Mixer
import Control.Monad (unless)

--- The game loop.     

go :: Game -> IO (Maybe Game)
go game1 = do
  SDL.initialize [SDL.InitAudio]
  Mixer.openAudio Mixer.defaultAudio 2048
  Mixer.loadMP3 "soundtrack.mp3" >>= Mixer.playMusic Mixer.Forever
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

-- Add the music to the game
sinSamples :: [Int16]
sinSamples =
  map (\n ->
         let t = fromIntegral n / 48000 :: Double
             freq = 440
         in round (fromIntegral (maxBound `div` 2 :: Int16) * sin (2 * pi * freq * t)))
      [0 :: Int32 ..]
  

main :: IO (Maybe Game)
main = do
  displayString "welcome"
  initialGameState <- resetGame a_tree
  go initialGameState
   