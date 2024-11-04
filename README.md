# Concept of the game
This is a text based adventure game. You are locked in a mine, represented as a binary tree, and your goal is to navigate through it, find useful items, pass through obstacles and eventually find your way out of it.

# Concepts implemented so far

## Understanding the game engine

### How to run the game
Download the files of the game, go to the directory of the game and run `ghci MineExplorer.hs -package containers base`.
then run the `main` function. If you have any issues with the packages follow the instructions and then rerun `:l MineExplorer.hs` inside `ghci`.

### How the engine is structured
The position within the map is stored as a BinTree Zip data type.

Each node of this tree contains a value of type NodeInfo, which is a record keeping track of an item and the number of visits at that node, respectively.


The game state is stored in a record type named `Game` which records useful information such as the position within the map, player's inventory, etc.

All functions that modify the game state (mainly `COMMANDS` of the player) are of type `Game -> IO Game`.

## [MineExplorer.hs](MineExplorer.hs)
*YOU DO NOT NEED TO TOUCH THIS FILE*

This file contains the main game loop, implemented as a function which takes in an initial game state and recursively applies itself to a new game state, obtained by modifying the original state according to the commands it receives.

Conditions to end the game: 
- The oxigen level reaches below 0
- The variable gameOver is set to True.


## [GameData.hs](GameData.hs)

This file declares the Game state variable, as well as the commands

## [Scripts.hs](Scripts.hs)

This file is mainly used for useful routines at each node such as getting the item at that node or showing the information at that node

## [Messages.hs](Messages.hs)

This file contains a dictionary with (key, value) pairs being `("shorthand of message" "the message that will be displayed")`.

To add a new message we add a line such as:
```haskell
_ Map insert "shorthand" "message you want" $ _
```











