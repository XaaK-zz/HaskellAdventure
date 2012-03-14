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
room1    = Room "You stand on a forest path.  It is close to nightfall and you don't want to be outside when night falls.  \nYou see the walls of a city to the north." [North,South] [2,3] defaultItemHandler ""
room2    = Room "Above you tower the massive stone walls of Dawn, the largest city-state in the region.  \nThe walls are protected by serious-looking guards, and a well-armed guard blocks your entrance to the city." [South] [1] defaultItemHandler ""
room3    = Room "You are in the forest.  " [North,East,South,West] [1,10,6,4] defaultItemHandler ""
room4    = Room "You are in a large clearing in the forest.  There is a large tree in the center of the clearing." [East] [3] defaultItemHandler ""
room5    = Room "You have climbed up to a perch in the tall tree. " [Down] [4] defaultItemHandler ""
room6    = Room "You stand outside the entrance to a dark cave." [North,South] [3,7] defaultItemHandler ""
room7    = Room "It is too dark to see." [North] [6] defaultItemHandler ""
room7Alt = Room "Are in a dark cave.  Strange sounds echo from the walls." [North,West] [6,8] defaultItemHandler ""
room8    = Room "Are in a dark cave.  The sound of water drips nearby." [East,South,West] [7,9,12] defaultItemHandler ""
room9    = Room "You are on one edge of a bottomless pit.  At the far side there is another passage, but you see no way across." [North] [8]
                    (\gn (Item _ desc _ _) -> if desc == "Shoes" then
                                                gn{ desc="You are on one edge of a bottomless pit.  At the far side there is another passage.",
                                                    roomTempOutput="You put on the magic shoes and feel yourself float above the ground.\n\n",exits=[North,South],adjacentRooms=[8,13]}
                                              else
                                                gn
                    ) ""
room10    = Room "You are at the edge of a large and majestic lake.  \nAn island sits in the center of the lake with some strange ruins on it.\nThere is a boat here - but it has no oars." [West] [3]
                    (\gn (Item _ desc _ _) -> if desc == "Branch" then
                                                gn{ desc="You are at the edge of a large and majestic lake.  \nAn island sits in the center of the lake with some strange ruins on it.\nThere is a boat here with a branch oar.",
                                                    roomTempOutput="You fashion the branch as a makeshift oar.\nYou think you can make it to the island now.\n\n",exits=[East,West],adjacentRooms=[11,3]}
                                              else
                                                gn
                    ) ""
room11    = Room "You are on a small island in the center of a large lake.\nThere are some ancient ruins here." [West] [10] defaultItemHandler ""
room12    = Room "The light from the lamp glitters off the many strange and beautiful crystal structures in this room." [East] [8] defaultItemHandler ""
room13    = Room "You have reached the mighty dragon's trasure chamber.  Atop a giant pile of treasure sleeps the great red beast." [North] [9] defaultItemHandler ""
room14    = Room "You have successfully made it into the city of Dawn.  Congratulations!" [South] [2] defaultItemHandler ""


startItemList = [Item 5 "Branch" "This is a large dead branch.  You think it could be used as an oar.\n\n" "",
                 Item 11 "Lamp" "A metal lamp.\n\n" "Off",
                 Item 12 "Shoes" "These shoes have a picture of wings on the side.\n\n" ""]

--Starting GameState
start = GameState {currentRoom = 1, items = startItemList, inventory = [], tempOutput="",
                   nodeList=[(1,room1),(2,room2),(3,room3),(4,room4),(5,room5),(6,room6),
                             (7,room7),(8,room8),(9,room9),(10,room10),(11,room11),
                             (12,room12),(13,room13),(14,room14)]}

defaultItemHandler gn item = gn