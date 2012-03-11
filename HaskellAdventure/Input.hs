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
import Data.Char

import MParserCombs

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
        gs{currentRoom = (currentRoom gs),tempOutput="You can't go that way.\n\n"}
    where
        room        = getRoom gs $ currentRoom gs
        roomExits   = exits room
        isValidExit = dir `elem` roomExits
        exitIndex   = elemIndex dir roomExits 

--Take/Pickup Commands
processInput gs (Get item) =
    if isValidLocationForItem then
        gs{items=newItemList,inventory=newInvItem:currentItems,tempOutput="You picked up the " ++ itemDesc newItem ++ ".\n\n"}
    else
        gs{tempOutput="I don't see a " ++ itemDesc newItem ++ " here.\n\n"}
    where
        newItem                = fromJust $ getItem gs (show item)
        currentRoomId          = currentRoom gs
        itemListInRoom         = (filter (\(Item itemLocation _ _ _) -> if itemLocation == currentRoomId then True else False) . items) gs
        isValidLocationForItem = (not . null . filter (\(Item _ desc _ _) -> if desc == itemDesc newItem then True else False)) itemListInRoom
        currentItems           = inventory gs
        newItemList            = filter (\tempItem -> itemLocation tempItem /= currentRoomId && itemDesc tempItem /= itemDesc newItem) (items gs)
        newInvItem             = fromJust (getItem gs (itemDesc newItem))

--Drop Commands
processInput gs (Drop item) =
    if haveItem then
        gs{items=newItemList,inventory=newInvList,tempOutput="You dropped the " ++ itemDesc (fromJust newItem) ++ ".\n\n"}
    else
        gs{tempOutput="You don't have a " ++ itemDesc (fromJust newItem) ++ ".\n\n"}
    where
        currentRoomId   = currentRoom gs
        newItem         = getItemFromInventory gs (show item)
        haveItem        = (not . isNothing) newItem
        newItemList     = (fromJust newItem){itemLocation = currentRoomId,itemLongDesc = (itemLongDesc (fromJust newItem))} : (items gs)
        newInvList      = filter (\tempItem -> itemLocation tempItem /= currentRoomId && itemDesc tempItem /= itemDesc (fromJust newItem)) (inventory gs)
    
--Inventory Command
processInput gs Inv = gs{tempOutput=showInventory $ inventory gs}

--Using Items
---Note: this uses the room specific function (useItem) for the current Room
--  This gives us back a new room, which we replace in the GameState NodeList
--  This allows us to make global changes to the game, such as opening locked doors in rooms
processInput gs (Use item) =
    if haveItem then
        gs{nodeList=newRoomList,tempOutput=roomTempOutput newRoom}
    else
        gs{tempOutput="You don't have a " ++ itemDesc newItem ++ ".\n\n"}
    where
        currentItems     = inventory gs
        haveItem         = (not . null . filter (\(Item _ desc _ _) -> if desc == itemDesc newItem then True else False)) currentItems
        newItem          = fromJust $ getItemFromInventory gs (show item)
        currentRoomId    = currentRoom gs
        currentRoomNode  = getRoom gs currentRoomId
        newRoom          = useItem currentRoomNode currentRoomNode newItem
        newRoomList      = (filter (\(roomId,_) -> if roomId == currentRoomId then False else True) (nodeList gs)) ++ [(currentRoomId,newRoom)]

--Looking at Items
processInput gs (Look item) =
    if haveItem then
        gs{tempOutput=itemLongDesc (fromJust newItem)}
    else
        gs{tempOutput="You don't have a " ++ itemDesc (fromJust newItem) ++ ".\n\n"}
    where
        currentItems  = inventory gs
        newItem       = getItemFromInventory gs $ show item
        haveItem      = (not . isNothing) newItem

--Invalid Input Command
processInput gs Invalid = gs{tempOutput="What???!\n"}

--Lighting Items
processInput gs (Light item) =
    if haveItem then
        gs{tempOutput="You lit the lamp.",inventory=(fromJust newItem){itemStatus="On"}:newInvList,nodeList=newRoomList}
    else
        gs{tempOutput="You don't have a " ++ itemDesc (fromJust newItem) ++ ".\n\n"}
    where
        currentItems  = inventory gs
        newItem       = getItemFromInventory gs $ show item
        haveItem      = (not . isNothing) newItem
        newInvList    = filter (\tempItem -> itemDesc tempItem /= itemDesc (fromJust newItem)) currentItems
        newRoomList   = (5,room5Alt) : filter (\(roomID,_) -> if roomID == 5 then False else True ) (nodeList gs)

--getComand
--This converts a user entered string to a Command data type
--  This parsing is crude - hopefully we can build a better parsing system...
getCommand :: String -> Command
getCommand input = do
                    let x = (parse command (map toLower input))
                    if null x then
                        Invalid
                    else
                        x !! 0

                    
