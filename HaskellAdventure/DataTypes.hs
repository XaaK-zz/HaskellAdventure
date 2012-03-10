------------------------------------------------------------------------
--DataTypes module for the HaskellAdventure game
--Copyright Zach Greenvoss 2012
--  Contains the data classes and types for the HaskellAdventure game
------------------------------------------------------------------------
module HaskellAdventure.DataTypes where

import MParserCombs

--Room Identifier - each room will have a unique id
type RoomId = Int
--Items
type ItemList = [Item]
--List of tuples representing the Rooms and their locations
type RoomList = [(RoomId,GameNode)]

--Movement directions in the world
data Direction = North | South | East | West | Up | Down
    deriving (Show,Eq) 

--GameNode is a type for representing the details about a room
data GameNode = Room {desc              :: String,
                      exits             :: [Direction],
                      adjacentRooms     :: [RoomId],
                      useItem           :: GameNode -> Item -> GameNode,
                      roomTempOutput    :: String }

--GameState is used to capture all revelant mutable data that can
--  change as the player moves through the game
data GameState = GameState {  currentRoom :: RoomId,
                              items       :: ItemList,
                              inventory   :: [Item],
                              tempOutput  :: String,
                              nodeList    :: RoomList}

--Item datatype to represent items in the game
--  These can be picked up, moved, and looked at
data Item = Item {itemLocation  :: RoomId,
                  itemDesc      :: String,
                  itemLongDesc  :: String,
                  itemStatus    :: String}
    deriving (Eq,Show)
    
--Commands allowed by the parser
data Command =   Go Direction
               | Get ItemDT
               | Inv
               | End
               | Use ItemDT
               | Look ItemDT
               | Drop ItemDT
               | Invalid
               | Light ItemDT
    deriving (Eq,Show)
    
data ItemDT = Key
            | Lamp 
    deriving (Eq,Show)

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
          (do tok "inv"; return (Inv)) `orelse`
          (do tok "quit"; return (End)) `orelse`
          (do dir <- directionModifier; return (Go dir)) `orelse`
          return Invalid

directionModifier =
           (do tok "north"; return North) `orelse`
           (do tok "south"; return South) `orelse`
           (do tok "east"; return East) `orelse`
           (do tok "west"; return West)
           
itemModifier = (do tok "key"; return Key) `orelse`
               (do tok "lamp"; return Lamp)