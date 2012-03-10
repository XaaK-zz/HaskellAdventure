------------------------------------------------------------------------
--Output module for the HaskellAdventure game
--Copyright Zach Greenvoss 2012
--  This module contains all functions related to displaying game to
--      the user
------------------------------------------------------------------------
module HaskellAdventure.Output where

import Data.Maybe
import qualified Data.IntMap as IntMap
import HaskellAdventure.DataTypes
import HaskellAdventure.Data

--describeState
--Accepts a GameState object and converts it to a string representation to
--  display to the user
describeState :: GameState -> String
describeState state = tempOutput state ++
                      (showRoom . (getRoom state) . currentRoom) state ++
                      (showItems (currentRoom state) (items state))
                      

--getRoom
--Accepts a GameState and a RoomId and extracts the requested Room from the GameState
getRoom :: GameState -> RoomId -> GameNode
getRoom gs roomId = getRoomInner roomId (nodeList gs)
    where
        getIndexForRoomId :: RoomId -> RoomList -> Int -> Int
        getIndexForRoomId incomingRoomId [] index              = 0
        getIndexForRoomId incomingRoomId ((roomId,_):rs) index = if roomId == incomingRoomId then
                                                                    index
                                                               else
                                                                    getIndexForRoomId incomingRoomId rs index+1
        getRoomInner :: RoomId -> RoomList -> GameNode
        getRoomInner roomId roomList = snd (roomList !! (getIndexForRoomId roomId roomList 0))

--getItemFromInventory
--Retrieves a specific item from the GameState object inventory list (based on the short description)
getItemFromInventory :: GameState -> String -> Maybe Item
getItemFromInventory gs itemDescription =
    if haveItem then
        Just (currentItems !! (getItemFromInventory' itemDescription currentItems 0))
    else
        Nothing
    where
        currentItems                                        = inventory gs
        haveItem                                            = (not . null . filter (\(Item _ desc _ _) -> if desc == itemDescription then
                                                                                                            True
                                                                                                        else
                                                                                                            False)
                                                                ) currentItems
        getItemFromInventory'                               :: String -> ItemList -> Int -> Int
        getItemFromInventory' itemDescription [] index      = 0
        getItemFromInventory' itemDescription (i:is) index  = if (itemDesc i) == itemDescription  then
                                                                 index
                                                              else
                                                                 getItemFromInventory' itemDescription is index+1

--getItem
--Retrieves a specific item from the GameState object (based on the short description)
getItem :: GameState -> String -> Maybe Item
getItem gs itemDescription =
    if validDescriptionForItem then
        Just (currentItems !! (getItem' itemDescription currentItems 0))
    else
        Nothing
    where
        currentItems                          = items gs
        validDescriptionForItem               = (not . null . filter (\(Item _ desc _ _) -> if desc == itemDescription then
                                                                                            True
                                                                                        else
                                                                                            False)
                                                ) currentItems
        getItem'                              :: String -> ItemList -> Int -> Int
        getItem' itemDescription [] index     = 0
        getItem' itemDescription (i:is) index = if (itemDesc i) == itemDescription  then
                                                    index
                                                else
                                                    getItem' itemDescription is index+1
             
--showRoom
--Accepts a GameNode Room and converts it to a string representation to
--  display to the user.
showRoom :: GameNode -> String
showRoom gn = desc gn ++ showExits gn

--showExits
--Accepts a GameNode Room and converts it to a string representation of
--  the exits from the room
--The secondary function is to ensure commas work correctly
showExits (Room _ [] _ _ _) = "\nThere are no exits."
showExits (Room a (e:es) _ f t) = "\nThe exits are: " ++ (show e) ++ showExits' (Room a es [] f t)
showExits' (Room _ [] _ _ _) = ""
showExits' (Room a (e:es) _ f t) = ", " ++ (show e) ++ showExits' (Room a es [] f t)

--showItems
--Accepts a RoomId (Int) and the current item list (which would normally be
--  extracted from the GameState object) and converts them to a string
--  representation of the current items in the room
showItems :: RoomId -> ItemList -> String
showItems currentRoom []     = ""
showItems currentRoom (i:is) =  if (itemLocation i) == currentRoom then
                                    "\nThere is a " ++ (itemDesc i) ++ " here." ++ showItems' currentRoom is
                                else
                                    showItems currentRoom is
                                        
showItems' currentRoom []                 = ""
showItems' currentRoom (i:is) = if (itemLocation i) == currentRoom then
                                    "There is a " ++ (itemDesc i) ++ " here.\n" ++ showItems' currentRoom is
                                else
                                    showItems' currentRoom is


--showInventory
--Accepts a list of Items and converts it to a String to display
--  to the user
showInventory :: [Item] -> String
showInventory []      = "\nYou have nothing."
showInventory (i:is)  = "\nYou have: " ++ (itemDesc i) ++ (showStatus i) ++ showInventory' is
showInventory' []     = ""
showInventory' (i:is) = "\n" ++ (itemDesc i) ++ (showStatus i) ++ showInventory' is

showStatus (Item _ _ _ s) = if null s then
                            ""
                          else
                            ".  It is currently: " ++ s


