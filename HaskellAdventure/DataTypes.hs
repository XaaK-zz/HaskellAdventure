------------------------------------------------------------------------
--DataTypes module for the HaskellAdventure game
--Copyright Zach Greenvoss 2012
--  Contains the data classes and types for the HaskellAdventure game
------------------------------------------------------------------------
module HaskellAdventure.DataTypes where

--Room Identifier - each room will have a unique id
type RoomId = Int
--Items
type Item = String
--List of tuples determining where each Item is located in the world
type ItemList = [(RoomId,Item)]
--
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

--Commands allowed by the parser
data Command =   Go Direction
               | Get Item
               | Inv
               | End
               | Use Item
    deriving (Show, Eq)