------------------------------------------------------------------------
--Input module for the HaskellAdventure game
--Copyright Zach Greenvoss 2012
--  This module contains all functions related to accepting, parsing,
--      and processing input from the user
------------------------------------------------------------------------
module HaskellAdventure.Input where

import Data.Maybe
import Data.List
import HaskellAdventure.DataTypes
import HaskellAdventure.Data
import HaskellAdventure.Output

--executeRoom
--This is the main control loop of the game
--Each successive command will invoke executeState recursively until the
--  game is completed.
executeState :: GameState -> IO () 
executeState gameState =
     do
        putStrLn $ describeState $ gameState 
        putStr "> "
        input <- getLine
        --rawInput <-  getInputLine "> "
        let inputCommand = getCommand input
        if inputCommand == End then
            return ()
        else
            executeState (processInput gameState inputCommand) >> return ()

--processInput
--There are a large number of processInput functions
--  Each one processes a different user command
--They each take a GameState and a Command and convert these to a new GameState,
--  thus potentially updating the state of the player in the world
processInput :: GameState -> Command -> GameState

--Movement Commands
processInput gs (Go dir) =
    if isValidExit then
        gs{currentRoom = ((adjacentRooms room) !! (fromJust exitIndex)),tempOutput=""}
    else
        gs{currentRoom = (currentRoom gs),tempOutput="\nYou can't go that way."}
    where
        room        = getRoomById $ currentRoom gs
        roomExits   = exits room
        isValidExit = dir `elem` roomExits
        exitIndex   = elemIndex dir roomExits 

--Take/Pickup Commands
processInput gs (Get item) =
    if isValidLocationForItem then
        gs{items=newItemList,inventory=item:currentItems,tempOutput=""}
    else
        gs{tempOutput="\nI don't see a " ++ item ++ " here."}
    where
        currentRoomId          = currentRoom gs
        isValidLocationForItem = (currentRoomId,item) `elem` (items gs)
        currentItems           = inventory gs
        newItemList            = filter (\(roomId,oldItem)->roomId /= currentRoomId && item /= oldItem) (items gs)

--Inventory Command
processInput gs Inv = gs{tempOutput=showInventory $ inventory gs}
        
--getComand
--This converts a user entered string to a Command data type
--  This parsing is crude - hopefully we can build a better parsing system...
getCommand :: String -> Command
getCommand input = do
                      if input == "north" then
                          Go North
                      else if input == "south" then
                          Go South
                      else if input == "east" then
                          Go East
                      else if input == "west" then
                          Go West
                      else if input == "get key" then
                          Get "Key"
                      else if input == "inv" then
                          Inv
                      else
                          End