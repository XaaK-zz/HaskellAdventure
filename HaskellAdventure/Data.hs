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
room1 = Room "You stand at the entrance to the castle." [North,South] [2,5] defaultItemHandler ""
room2 = Room "The gatehouse.  Behind lies the portcullis - ahead lies the courtyard of the castle." [North, South] [3,1] defaultItemHandler ""
room3 = Room "End of the line.  There is a locked door to the north." [South] [2]
                (\gn (Item _ desc _ _) -> if desc == "Key" then
                                             gn{desc="End of the line.  There is an open door here.",roomTempOutput="The door opens...\n\n",exits=[North,South],adjacentRooms=[4,2]}
                                          else
                                             gn
                ) ""
room4 = Room "You made it to the treasure room!" [South] [3] defaultItemHandler ""
room5 = Room "It is too dark to see." [North] [1] defaultItemHandler ""
room5Alt = Room "You are in a dark passageway." [North,South] [1,6] defaultItemHandler ""
room6 = Room "You are at the entrance to the sewer." [North] [5] defaultItemHandler ""

--room1 = Room "You stand on a forest path.  It is close to nightfall and you don't want to be outside when night falls.  You see the walls of a city to the north." [North,South] [2,3] defaultItemHandler ""
--room2 = Room "Above you tower the massive stone walls of Dawn, the largest city-state in the region.  The walls are protected by serious faced guards, and a well-armed guard blocks your entrance to the city." [South] [1] defaultItemHandler ""
--room3 = Room "You are in the forest.  " [North,East,South,West] [1,10,6,4] defaultItemHandler ""
--room4 = Room "You are in a large clearing in the forest.  There is a large tree in the center of the clearing." [East] [3] defaultItemHandler ""
--room5 = Room "You have climbed up to a perch in the tall tree.  There is a large dead branch here." [Down] [4] defaultItemHandler ""
--room6 = Room "You stand outside the entrance to a dark cave." [North,South] [3,7] defaultItemHandler ""
--room7 = Room "Are in a dark cave.  Strange sounds echo from the walls." [North,West] [6,8] defaultItemHandler ""
--room8 = Room "Are in a dark cave.  The sound of water drips nearby." [East,South,West] [7,9,12] defaultItemHandler ""



startItemList = [Item 3 "Key" "A metal key, rusted and pitted with age.\n\n" "",
                 Item 1 "Lamp" "A metal lamp.\n\n" "Off"]

--Starting GameState
start = GameState {currentRoom = 1, items = startItemList, inventory = [], tempOutput="",
                   nodeList=[(1,room1),(2,room2),(3,room3),(4,room4),(5,room5),(6,room6)]}

defaultItemHandler gn item = gn