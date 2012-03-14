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

--executeState
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
        newRoomList   = (7,room7Alt) : filter (\(roomID,_) -> if roomID == 7 then False else True ) (nodeList gs)

--Talking to things
processInput gs (Talk thing) =
    if currentRoom gs == 2 && thing == "guard" then
        gs{tempOutput="The guard says you cannot enter the city without a gift for the king.  Only a golden egg will do.\n\n"}
    else if currentRoom gs == 13 && thing == "dragon" then
        gs{tempOutput="\"You want a piece of my great treasure?  Then you will need to answer a simple riddle:\n\tA box without hinges, key, or lid,\n\tYet golden treasure inside is hid.\n\""}
    else
        gs{tempOutput="I don't see a " ++ thing ++ " around here.\n"}

--Climbing things
processInput gs (Climb thing) =
    if currentRoom gs == 4 && thing == "tree" then
        gs{tempOutput="You climb the tree with difficulty.\n\n",currentRoom=5}
    else
        gs{tempOutput="I don't see a " ++ thing ++ " around here.\n"}

        
--getComand
--This converts a user entered string to a Command data type
getCommand :: String -> Command
getCommand input = do
                    let x = (parse command (map toLower input))
                    if null x then
                        Invalid
                    else
                        x !! 0

--Parser details
command             :: Parser Command
directionModifier   :: Parser Direction
itemModifier        :: Parser ItemDT

command = (do tok "go "; dir <- directionModifier; return (Go dir)) `orelse`
          (do tok "get "; itemTemp <- itemModifier; return (Get itemTemp)) `orelse`
          (do tok "use "; itemTemp <- itemModifier; return (Use itemTemp)) `orelse`
          (do tok "look "; itemTemp <- itemModifier; return (Look itemTemp)) `orelse`
          (do tok "drop "; itemTemp <- itemModifier; return (Drop itemTemp)) `orelse`
          (do tok "light "; itemTemp <- itemModifier; return (Light itemTemp)) `orelse`
          (do tok "talk "; talkTemp <- many (sat isLetter); return (Talk talkTemp)) `orelse`
          (do tok "climb "; climbTemp <- many (sat isLetter); return (Climb climbTemp)) `orelse`
          (do tok "inv"; return (Inv)) `orelse`
          (do tok "quit"; return (End)) `orelse`
          (do dir <- directionModifier; return (Go dir)) `orelse`
          return Invalid

directionModifier =
           (do tok "north"; return North) `orelse`
           (do tok "south"; return South) `orelse`
           (do tok "east"; return East) `orelse`
           (do tok "west"; return West) `orelse`
           (do tok "down"; return Down) `orelse`
           (do tok "up"; return Up)
           
itemModifier = (do tok "branch"; return Branch) `orelse`
               (do tok "lamp"; return Lamp) `orelse`
               (do tok "shoes"; return Shoes) `orelse`
               (do tok "magic shoes"; return Shoes)

                    
