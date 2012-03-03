------------------------------------------------------------------------
--Data module for the HaskellAdventure game
--Copyright Zach Greenvoss 2012
--  This module contains the core data for the game - list of rooms, items,
--      starting location, and a way to query for rooms
------------------------------------------------------------------------
module HaskellAdventure.Data where

import HaskellAdventure.DataTypes
import qualified Data.IntMap as IntMap
import Data.Maybe

--List of GameNodes
room1 = Room "You stand at the entrance to the castle." [North] [2]
room2 = Room "The gatehouse.  Behind lies the portcullis - ahead lies the courtyard of the castle." [North, South] [3,1]
room3 = Room "End of the line" [South] [2]

--getRoomById
--Function for extracting a room for a given Id
getRoomById :: RoomId -> GameNode
getRoomById id = if isNothing $ IntMap.lookup id roomList then
                    room1
                 else
                    fromJust $ IntMap.lookup id roomList
                 where
                    roomList = IntMap.fromList [(1,room1),(2,room2),(3,room3)]

--Starting GameState
start = GameState {currentRoom = 1, items = [(3,"Key")], inventory = [], tempOutput=""}
