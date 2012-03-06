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
showItems currentRoom []                 = ""
showItems currentRoom ((roomId,item):es) = if currentRoom == roomId then
                                                "\nThere is a " ++ item ++ " here." ++ showItems' currentRoom es
                                           else
                                                showItems currentRoom es
showItems' currentRoom []                 = ""
showItems' currentRoom ((roomId,item):es) = if currentRoom == roomId then
                                                "There is a " ++ item ++ " here.\n" ++ showItems' currentRoom es
                                           else
                                                showItems' currentRoom es

--showInventory
--Accepts a list of Items and converts it to a String to display
--  to the user
showInventory :: [Item] -> String
showInventory []      = "\nYou have nothing."
showInventory (i:is)  = "\nYou have: " ++ i ++ showInventory' is
showInventory' []     = ""
showInventory' (i:is) = "\n" ++ i ++ showInventory' is


