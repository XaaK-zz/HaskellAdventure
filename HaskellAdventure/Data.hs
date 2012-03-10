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
room1 = Room "You stand at the entrance to the castle." [North] [2] defaultItemHandler ""
room2 = Room "The gatehouse.  Behind lies the portcullis - ahead lies the courtyard of the castle." [North, South] [3,1] defaultItemHandler ""
room3 = Room "End of the line.  There is a locked door to the north." [South] [2]
                (\gn (Item _ desc _ _) -> if desc == "Key" then
                                            gn{desc="End of the line.  There is an open door here.",roomTempOutput="The door opens...\n\n",exits=[North,South],adjacentRooms=[4,2]}
                                        else
                                           gn
                ) ""
room4 = Room "You made it to the treasure room!" [South] [3] defaultItemHandler ""

startItemList = [Item 3 "Key" "A metal key, rusted and pitted with age.\n\n" "",
                 Item 1 "Lamp" "A metal lamp.\n\n" "Off"]

--Starting GameState
start = GameState {currentRoom = 1, items = startItemList, inventory = [], tempOutput="", nodeList=[(1,room1),(2,room2),(3,room3),(4,room4)]}

defaultItemHandler gn item = gn